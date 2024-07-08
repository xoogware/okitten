open Lwt
open Utils
open Commands.Shard

type t =
  { id : int
  ; last_heartbeat_sent : float option
  ; last_heartbeat_ack_at : float option
  ; last_heartbeat_was_acknowledged : bool
  ; heartbeat_interval : float option
  ; on_get_appid : string -> unit
  ; seq : int option
  ; session_id : string option
  ; started_at : float
  ; token : string
  ; ws_url : string
  ; ws_conn : Websocket_lwt_unix.conn
  ; intents : int
  ; push_cmd : command option -> unit
  ; cmd : command Lwt_stream.t
  ; push_to_coordinator : Commands.Coordinator.command option -> unit
  ; presence : Presence.t option
  }

let connect_gateway ~ws_url =
  let open Websocket_lwt_unix in
  let sanitized = Str.replace_first (Str.regexp "^wss") "https" ws_url in
  let uri = Uri.of_string sanitized in
  let%lwt resolved_uri = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx resolved_uri in
  connect ~ctx client uri
;;

let init ~id ~token ~intents ~push_cmd ~cmd ~push_to_coordinator ~ws_url ~with_presence =
  let%lwt ws_conn = connect_gateway ~ws_url in
  return
    { id
    ; last_heartbeat_sent = None
    ; last_heartbeat_ack_at = None
    ; last_heartbeat_was_acknowledged = false
    ; heartbeat_interval = None
    ; on_get_appid = (fun _ -> ())
    ; seq = None
    ; session_id = None
    ; started_at = 0.
    ; token
    ; ws_url
    ; ws_conn
    ; intents
    ; push_cmd
    ; cmd
    ; push_to_coordinator
    ; presence = with_presence
    }
;;

let send_identify ~total_shards shard =
  let open Gateway in
  Logs.debug (fun m -> m "Identifying shard %d" shard.id);
  let identify =
    { token = shard.token
    ; properties = { os = "unix"; browser = "okitten"; device = "okitten" }
    ; compress = false
    ; large_threshold = 50
    ; shard = shard.id, total_shards
    ; intents = shard.intents
    ; presence = shard.presence
    }
    |> payload_of ~op:Identify
    |> yojson_of_payload yojson_of_identify
    |> Yojson.Safe.to_string
  in
  let frame = Websocket.Frame.create ~opcode:Text ~content:identify () in
  let%lwt _ = Websocket_lwt_unix.write shard.ws_conn frame in
  return shard
;;

let handle_dispatch ~json (s : t) =
  let module YSU = Yojson.Safe.Util in
  let open Gateway in
  let dispatch_op = json |> YSU.member "t" |> Dispatch.event_of_yojson in
  let data = json |> YSU.member "d" in
  match dispatch_op with
  | Hello ->
    let data = hello_of_yojson data in
    return { s with heartbeat_interval = Some (float_of_int data.heartbeat_interval) }
  | e ->
    Logs.warn (fun m -> m "Got unhandled Dispatch event %s" @@ Dispatch.event_to_string e);
    return s
;;

let handle_packet ~packet ~cancellation_semaphore s =
  let open Websocket in
  let module YSU = Yojson.Safe.Util in
  let Frame.{ opcode; content; _ } = packet in
  match opcode with
  | Close ->
    let%lwt _ = Lwt_mvar.put cancellation_semaphore () in
    return s
  | Text ->
    let json = Yojson.Safe.from_string content in
    let opcode = json |> YSU.member "op" |> Gateway.Opcode.t_of_yojson in
    let data = json |> YSU.member "d" in
    let seq = json |> YSU.member "s" |> option_of_yojson int_of_yojson in
    let s =
      match seq with
      | Some seq ->
        Logs.debug (fun m -> m "Seq adjusted to %d" seq);
        { s with seq = Some seq }
      | None -> s
    in
    (match opcode with
     | Hello ->
       let data = Gateway.hello_of_yojson data in
       Logs.debug (fun m ->
         m "Shard %d heartbeating every %f seconds" s.id
         @@ (float_of_int data.heartbeat_interval /. 1000.));
       return { s with heartbeat_interval = Some (float_of_int data.heartbeat_interval) }
     | Dispatch -> handle_dispatch ~json s
     | HeartbeatAck -> return { s with last_heartbeat_ack_at = Some (Unix.time ()) }
     | op ->
       Logs.warn (fun m ->
         m "Received unimplemented opcode %s" @@ Gateway.Opcode.to_string op);
       return s)
  | _ ->
    Logs.err (fun m -> m "Got unexpected opcode %s" (Frame.Opcode.to_string opcode));
    return s
;;

let rec handle_gateway_rx ~id ~push ~ws_conn ~cancellation_semaphore =
  if Lwt_mvar.is_empty cancellation_semaphore = false
  then return ()
  else
    let open Websocket in
    let%lwt packet = Websocket_lwt_unix.read ws_conn in
    push (Some (Handle packet));
    match packet with
    | { Frame.opcode = Close; content; _ } ->
      Logs.debug (fun m -> m "Connection closed on shard %d unexpectedly" id);
      return ()
    | _ -> handle_gateway_rx ~id ~push ~ws_conn ~cancellation_semaphore
;;

let send_heartbeat s =
  Logs.debug (fun m -> m "Shard %d OK to heartbeat. seq=%d" s.id (s.seq /// -1));
  let open Websocket in
  let open Gateway in
  let content =
    s.seq
    |> payload_of ~op:Heartbeat
    |> yojson_of_payload yojson_of_heartbeat
    |> Yojson.Safe.to_string
  in
  let%lwt _ =
    Frame.create ~opcode:Text ~content () |> Websocket_lwt_unix.write s.ws_conn
  in
  return { s with last_heartbeat_sent = Some (Unix.time ()) }
;;

let start shard =
  let rec event_loop ~cancellation_semaphore s =
    let rec handle_cmds' s cmds =
      match cmds with
      | [] -> return @@ Some s
      | Identify total_shards :: xs ->
        let%lwt s = send_identify ~total_shards s in
        Lwt.async (fun () ->
          handle_gateway_rx
            ~id:shard.id
            ~push:shard.push_cmd
            ~ws_conn:shard.ws_conn
            ~cancellation_semaphore);
        handle_cmds' s xs
      | Handle packet :: xs ->
        let%lwt s = handle_packet ~packet ~cancellation_semaphore s in
        handle_cmds' s xs
      | Shutdown :: _ ->
        let%lwt _ = Lwt_mvar.put cancellation_semaphore () in
        return None
    in
    let%lwt s =
      match s.heartbeat_interval with
      | Some heartbeat_interval ->
        let now = Unix.time () in
        (* heartbeat interval is sent in ms *)
        if now -. (s.last_heartbeat_sent /// 0.) < heartbeat_interval /. 1000.
        then return s
        else send_heartbeat s
      | None -> return s
    in
    s.cmd
    |> Lwt_stream.get_available
    |> handle_cmds' s
    >>= function
    | Some s ->
      let%lwt _ = Lwt_unix.sleep 0.1 in
      event_loop ~cancellation_semaphore s
    | None -> return ()
  in
  Logs.debug (fun m -> m "Starting shard with ID %d." shard.id);
  (* this will be filled with a unit value to signal cancellation to gateway listener *)
  let cancellation_semaphore = Lwt_mvar.create_empty () in
  event_loop ~cancellation_semaphore shard
;;

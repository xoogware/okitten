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
  ; push_cmd : command -> unit
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

let now () = Unix.gettimeofday () *. 1000.

let send_payload ?op s p =
  let p = Yojson.Safe.to_string p in
  let frame =
    Websocket.Frame.create ~opcode:(op /// Websocket.Frame.Opcode.Text) ~content:p ()
  in
  Websocket_lwt_unix.write s.ws_conn frame
;;

let send_identify ~total_shards shard =
  let open Gateway in
  Logs.debug (fun m -> m "Identifying shard %d" shard.id);
  let%lwt _ =
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
    |> send_payload shard
  in
  return shard
;;

let handle_dispatch ~json (s : t) =
  let module YSU = Yojson.Safe.Util in
  let open Gateway in
  let dispatch_op = json |> YSU.member "t" |> Dispatch.event_of_yojson in
  let _data = json |> YSU.member "d" in
  match dispatch_op with
  | e ->
    Logs.warn (fun m -> m "Got unhandled Dispatch event %s" @@ Dispatch.event_to_string e);
    return s
;;

let latency shard =
  match shard.last_heartbeat_sent, shard.last_heartbeat_ack_at with
  | Some s, Some r -> r -. s
  | a, b -> 0.
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
     | HeartbeatAck ->
       let s =
         { s with
           last_heartbeat_ack_at = Some (now ())
         ; last_heartbeat_was_acknowledged = true
         }
       in
       Logs.debug (fun m -> m "Shard %d heartbeat acked (%fms)" s.id (latency s));
       return s
     | op ->
       Logs.warn (fun m ->
         m "Received unimplemented opcode %s" @@ Gateway.Opcode.to_string op);
       return s)
  | _ ->
    Logs.err (fun m -> m "Got unexpected opcode %s" (Frame.Opcode.to_string opcode));
    return s
;;

let rec handle_gateway_rx ~cancellation_semaphore s =
  if Lwt_mvar.is_empty cancellation_semaphore = false
  then return ()
  else
    let open Websocket in
    let%lwt packet = Websocket_lwt_unix.read s.ws_conn in
    s.push_cmd (Handle packet);
    match packet with
    | { Frame.opcode = Close; _ } ->
      Logs.debug (fun m -> m "Connection closed on shard %d unexpectedly" s.id);
      return ()
    | _ -> handle_gateway_rx ~cancellation_semaphore s
;;

let try_heartbeat s =
  let do_heartbeat s =
    if Option.is_some s.last_heartbeat_sent && not s.last_heartbeat_was_acknowledged
    then (
      Logs.debug (fun m -> m "Shard %d heartbeat was missed, shutting down" s.id);
      s.push_cmd Shutdown;
      return s)
    else (
      Logs.debug (fun m -> m "Shard %d OK to heartbeat. seq=%d" s.id (s.seq /// -1));
      let open Gateway in
      let%lwt _ =
        s.seq
        |> payload_of ~op:Heartbeat
        |> yojson_of_payload yojson_of_heartbeat
        |> send_payload s
      in
      return
        { s with
          last_heartbeat_sent = Some (now ())
        ; last_heartbeat_was_acknowledged = false
        })
  in
  match s.heartbeat_interval with
  | Some heartbeat_interval ->
    (* heartbeat interval is sent in ms *)
    if now () -. (s.last_heartbeat_sent /// 0.) < heartbeat_interval
    then return s
    else do_heartbeat s
  | None -> return s
;;

let set_presence presence shard = shard.push_cmd (SetPresence presence)
let id s = s.id
let cmds s = s.cmd

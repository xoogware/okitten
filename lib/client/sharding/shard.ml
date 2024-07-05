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
  ; seq : int
  ; session_id : string option
  ; started_at : float
  ; token : string
  ; ws_url : string
  ; ws_conn : Websocket_lwt_unix.conn
  ; intents : int
  ; push_cmd : command option -> unit
  ; cmd : command Lwt_stream.t
  ; push_to_coordinator : Commands.Coordinator.command option -> unit
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

let init ~id ~token ~intents ~push_cmd ~cmd ~push_to_coordinator ~ws_url =
  let%lwt ws_conn = connect_gateway ~ws_url in
  return
    { id
    ; last_heartbeat_sent = None
    ; last_heartbeat_ack_at = None
    ; last_heartbeat_was_acknowledged = false
    ; heartbeat_interval = None
    ; on_get_appid = (fun _ -> ())
    ; seq = 0
    ; session_id = None
    ; started_at = 0.
    ; token
    ; ws_url
    ; ws_conn
    ; intents
    ; push_cmd
    ; cmd
    ; push_to_coordinator
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
    }
    |> payload_of ~op:`Identify
    |> payload_to_yojson identify_to_yojson
    |> Yojson.Safe.to_string
  in
  let frame = Websocket.Frame.create ~opcode:Text ~content:identify () in
  let%lwt _ = Websocket_lwt_unix.write shard.ws_conn frame in
  return shard
;;

let handle_packet packet s = return s

let start shard =
  let rec handle_gateway_rx ~id ~push ~ws_conn ~cancellation_semaphore =
    if Lwt_mvar.is_empty cancellation_semaphore = false
    then return ()
    else
      let open Websocket in
      let%lwt packet = Websocket_lwt_unix.read ws_conn in
      push (Some (Handle packet));
      match packet with
      | { Frame.opcode = Close; content; _ } ->
        Logs.debug (fun m -> m "Connection closed on shard %d for %s" id content);
        return ()
      | _ -> handle_gateway_rx ~id ~push ~ws_conn ~cancellation_semaphore
  in
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
        let%lwt s = handle_packet packet s in
        handle_cmds' s xs
      | Shutdown :: _ ->
        let%lwt _ = Lwt_mvar.put cancellation_semaphore () in
        return None
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

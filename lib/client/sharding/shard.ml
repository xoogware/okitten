open Lwt
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
  ; ws_conn : Websocket_lwt_unix.conn option
  ; intents : int
  ; cmd : command Lwt_stream.t
  ; push_to_coordinator : Commands.Coordinator.command option -> unit
  }

let init ~id ~token ~intents ~cmd ~push_to_coordinator ~ws_url =
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
  ; ws_conn = None
  ; intents
  ; cmd
  ; push_to_coordinator
  }
;;

let connect_gateway shard =
  let open Websocket_lwt_unix in
  let sanitized = Str.replace_first (Str.regexp "^wss") "https" shard.ws_url in
  let uri = Uri.of_string sanitized in
  let%lwt resolved_uri = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx resolved_uri in
  connect ~ctx client uri
;;

let start shard =
  let rec event_loop s =
    match%lwt Lwt_stream.get s.cmd with
    | None ->
      let%lwt _ = Lwt_unix.sleep 0.1 in
      event_loop s
    | Some cmd ->
      (match cmd with
       | Identify ->
         Logs.debug (fun m -> m "Identifying shard %d." shard.id);
         let%lwt ws_conn = connect_gateway shard in
         let s = { s with ws_conn = Some ws_conn } in
         event_loop s
       | Shutdown -> return ())
  in
  Logs.debug (fun m -> m "Starting shard with ID %d." shard.id);
  event_loop shard
;;

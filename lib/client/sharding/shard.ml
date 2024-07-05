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

let init ~id ~token ~intents ~cmd ~push_to_coordinator ~ws_url =
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

let start shard =
  let handle_gateway_rx s =
    let open Websocket in
    Websocket_lwt_unix.read s.ws_conn
    >|= function
    | { Frame.opcode = Close; content; _ } ->
      Logs.debug (fun m -> m "Connection closed on shard %d" s.id);
      None
    | _ -> Some s
  in
  let handle_cmds ~event_loop s =
    match%lwt Lwt_stream.get s.cmd with
    | None ->
      let%lwt _ = Lwt_unix.sleep 0.1 in
      event_loop s
    | Some cmd ->
      (match cmd with
       | Identify total_shards ->
         let%lwt s = send_identify ~total_shards s in
         event_loop s
       | Shutdown -> return ())
  in
  let rec event_loop s =
    let%lwt s = handle_gateway_rx s in
    match s with
    | None -> return ()
    | Some s -> handle_cmds ~event_loop s
  in
  Logs.debug (fun m -> m "Starting shard with ID %d." shard.id);
  event_loop shard
;;

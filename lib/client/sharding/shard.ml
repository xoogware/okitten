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
  ; intents : int
  ; cmd : command Lwt_stream.t
  ; push_to_coordinator : Commands.Coordinator.command option -> unit
  }

let init ~id ~token ~intents ~cmd ~push_to_coordinator =
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
  ; ws_url = ""
  ; intents
  ; cmd
  ; push_to_coordinator
  }
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
         event_loop s
       | Shutdown -> return ())
  in
  Logs.debug (fun m -> m "Starting shard with ID %d." shard.id);
  event_loop shard
;;

type t

val init
  :  id:int
  -> token:string
  -> intents:int
  -> push_to_coordinator:(Coordinator_commands.t option -> unit)
  -> ws_url:string
  -> with_presence:Presence.t option
  -> t Lwt.t

val send_payload : ?op:Websocket.Frame.Opcode.t -> t -> Yojson.Safe.t -> unit Lwt.t
val send_identify : total_shards:int -> t -> t Lwt.t

val listen
  :  push_packet:(Websocket.Frame.t option -> unit)
  -> cancellation_semaphore:unit Lwt_mvar.t
  -> t
  -> unit Lwt.t

val try_heartbeat : cancellation_semaphore:unit Lwt_mvar.t -> t -> t Lwt.t

val handle_packet
  :  packet:Websocket.Frame.t
  -> cancellation_semaphore:unit Lwt_mvar.t
  -> t
  -> t Lwt.t

val id : t -> int
val latency : t -> float
(*val set_presence : Presence.t -> t -> unit*)

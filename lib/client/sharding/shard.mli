type t

val init
  :  id:int
  -> token:string
  -> intents:int
  -> push_cmd:(Commands.Shard.command -> unit)
  -> cmd:Commands.Shard.command Lwt_stream.t
  -> push_to_coordinator:(Commands.Coordinator.command option -> unit)
  -> ws_url:string
  -> with_presence:Presence.t option
  -> t Lwt.t

val send_payload : ?op:Websocket.Frame.Opcode.t -> t -> Yojson.Safe.t -> unit Lwt.t
val send_identify : total_shards:int -> t -> t Lwt.t
val handle_gateway_rx : cancellation_semaphore:unit Lwt_mvar.t -> t -> unit Lwt.t
val try_heartbeat : t -> t Lwt.t

val handle_packet
  :  packet:Websocket.Frame.t
  -> cancellation_semaphore:unit Lwt_mvar.t
  -> t
  -> t Lwt.t

val id : t -> int
val cmds : t -> Commands.Shard.command Lwt_stream.t
val latency : t -> float
val set_presence : Presence.t -> t -> unit

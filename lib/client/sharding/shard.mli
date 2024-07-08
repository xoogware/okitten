type t

val init
  :  id:int
  -> token:string
  -> intents:int
  -> push_cmd:(Commands.Shard.command option -> unit)
  -> cmd:Commands.Shard.command Lwt_stream.t
  -> push_to_coordinator:(Commands.Coordinator.command option -> unit)
  -> ws_url:string
  -> with_presence:Presence.t option
  -> t Lwt.t

val start : t -> unit Lwt.t

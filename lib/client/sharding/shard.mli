type t

val init
  :  id:int
  -> token:string
  -> intents:int
  -> cmd:Commands.Shard.command Lwt_stream.t
  -> push_to_coordinator:(Commands.Coordinator.command option -> unit)
  -> ws_url:string
  -> t

val start : t -> unit Lwt.t

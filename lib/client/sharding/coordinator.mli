type t

(** Initializes a new {!Coordinator.t}, returning it. *)
val init : http:Http.t -> token:string -> intents:int -> event_handler:EventHandler.t -> t

(** Starts the coordinator's event loop, returning the push command. *)
val run : t -> Coordinator_commands.t option -> unit

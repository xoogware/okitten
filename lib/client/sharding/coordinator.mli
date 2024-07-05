type t

(** Initializes a new {!Coordinator.t}, returning it. *)
val init : token:string -> t

(** Starts the coordinator's event loop, returning the push command. *)
val run : t -> Commands.Coordinator.command option -> unit

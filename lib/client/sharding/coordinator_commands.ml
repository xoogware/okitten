type response = Ok

type t =
  | Spawn of int * string * Presence.t option * response Lwt_mvar.t
  | Event of EventHandler.event
  (** Spawns the given number of shards with the given websocket url. *)
  | Shutdown of int (** Shuts the shard with the given ID down. *)
  | ShutdownAll

type t =
  | Identify of int (** Total shards to identify out of *)
  | Shutdown
  | SetPresence of Presence.t
  | GetShard of int * Shard.t Lwt_mvar.t

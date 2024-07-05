module Coordinator = struct
  type response = Ok

  type command =
    | Spawn of int * response Lwt_mvar.t (** Spawns the given number of shards. *)
    | Shutdown of int (** Shuts the shard with the given ID down. *)
end

module Shard = struct
  type command = Shutdown
end

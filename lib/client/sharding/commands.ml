module Coordinator = struct
  type response = Ok

  type command =
    | Spawn of int * string * response Lwt_mvar.t
    (** Spawns the given number of shards with the given websocket url. *)
    | Shutdown of int (** Shuts the shard with the given ID down. *)
    | ShutdownAll
end

module Shard = struct
  type command =
    | Identify of int (** Total shards to identify out of *)
    | Shutdown
    | Handle of Websocket.Frame.t
end

open Lwt

type t =
  { http : Http.t
  ; push_coordinator_cmd : Commands.Coordinator.command option -> unit
  ; token : string
  ; intents : int
  }

module ClientBuilder = struct
  type t =
    { token : string
    ; intents : int
    }

  let init ~token = { token; intents = 0 }
  let set_intents intents builder = { builder with intents }

  let build b =
    let http = Http.Builder.create ~token:b.token |> Http.Builder.build in
    let coordinator = Coordinator.init ~token:b.token in
    let push_coordinator_cmd = Coordinator.run coordinator in
    return { http; push_coordinator_cmd; token = b.token; intents = b.intents }
  ;;
end

let start ?shards c =
  let initialize_shards _start_shard total_shards c =
    let res_pipe = Lwt_mvar.create_empty () in
    c.push_coordinator_cmd (Some (Spawn (total_shards, res_pipe)));
    let%lwt _ = Lwt_mvar.take res_pipe in
    return c
  in
  match shards with
  | None ->
    Logs.info (fun m -> m "Shard parameter not passed; starting one shard.");
    return @@ Ok (initialize_shards 0 1 c)
  | Some `Autosharded ->
    Logs.info (fun m -> m "Autosharding.");
    (match%lwt Http.get_bot_gateway c.http with
     | Ok g ->
       Logs.info (fun m -> m "Using %d shards." g.shards);
       return @@ Ok (initialize_shards 0 g.shards c)
     | Error e ->
       Logs.err (fun m -> m "Failed to fetch gateway info: %s. stopping" e);
       return @@ Error e)
  | Some (`Manual shards) ->
    Logs.info (fun m -> m "Using %d shards." shards);
    return @@ Ok (initialize_shards 0 shards c)
;;

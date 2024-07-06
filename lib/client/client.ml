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
  Logs.info (fun m -> m "Running OKitten v%s." @@ Version.get ());
  let initialize_shards ~count ~ws_url c =
    let res_pipe = Lwt_mvar.create_empty () in
    c.push_coordinator_cmd (Some (Spawn (count, ws_url, res_pipe)));
    let%lwt _ = Lwt_mvar.take res_pipe in
    return c
  in
  Logs.debug (fun m -> m "Fetching Gateway info");
  let%lwt g = Http.get_bot_gateway c.http in
  match shards with
  | None ->
    Logs.info (fun m -> m "Shard parameter not passed; starting one shard.");
    return @@ Ok (initialize_shards ~count:1 ~ws_url:g.url c)
  | Some `Autosharded ->
    Logs.info (fun m -> m "Autosharding; using %d shards." g.shards);
    return @@ Ok (initialize_shards ~count:g.shards ~ws_url:g.url c)
  | Some (`Manual shards) ->
    Logs.info (fun m -> m "Using %d shards." shards);
    return @@ Ok (initialize_shards ~count:shards ~ws_url:g.url c)
;;

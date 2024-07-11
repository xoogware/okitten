open Lwt

type t =
  { http : Http.t
  ; push_coordinator_cmd : Coordinator_commands.t option -> unit
  ; token : string
  ; intents : int
  }

module ClientBuilder = struct
  type t =
    { token : string
    ; intents : int
    ; event_handler : EventHandler.t
    }

  let init = { token = ""; intents = 0; event_handler = EventHandler.init () }
  let set_token token builder = { builder with token }
  let set_intents intents builder = { builder with intents }
  let set_event_handler event_handler builder = { builder with event_handler }

  let build b =
    let http = Http.Builder.create ~token:b.token |> Http.Builder.build in
    let coordinator =
      Coordinator.init
        ~http
        ~token:b.token
        ~intents:b.intents
        ~event_handler:b.event_handler
    in
    let push_coordinator_cmd = Coordinator.run coordinator in
    return { http; push_coordinator_cmd; token = b.token; intents = b.intents }
  ;;
end

let start ?shards ?with_presence c =
  Logs.info (fun m -> m "Running OKitten v%s." @@ Version.get ());
  let initialize_shards ~count ~ws_url ~with_presence c =
    let res_pipe = Lwt_mvar.create_empty () in
    c.push_coordinator_cmd (Some (Spawn (count, ws_url, with_presence, res_pipe)));
    let%lwt _ = Lwt_mvar.take res_pipe in
    return c
  in
  Logs.debug (fun m -> m "Fetching Gateway info");
  let%lwt g = Http.get_bot_gateway c.http in
  let%lwt c =
    match shards with
    | None ->
      Logs.info (fun m -> m "Shard parameter not passed; starting one shard.");
      initialize_shards ~count:1 ~ws_url:g.url ~with_presence c
    | Some `Autosharded ->
      Logs.info (fun m -> m "Autosharding; using %d shards." g.shards);
      initialize_shards ~count:g.shards ~ws_url:g.url ~with_presence c
    | Some (`Manual shards) ->
      Logs.info (fun m -> m "Using %d shards." shards);
      initialize_shards ~count:shards ~ws_url:g.url ~with_presence c
  in
  c.push_coordinator_cmd (Some (Event Ready));
  return c
;;

open Lwt
open Utils

type tracked_shard = int * (Runner_commands.t -> unit) * bool

type t =
  { cmd_stream : Coordinator_commands.t Lwt_stream.t
  ; push_cmd : Coordinator_commands.t option -> unit
  ; last_identify : float option
  ; token : string
  ; intents : int
  ; shards :
      tracked_shard list (* TODO: use a map for this instead for faster replacements *)
  ; event_handler : EventHandler.t
  }

let init ~token ~intents ~event_handler =
  let cmd_stream, push_cmd = Lwt_stream.create () in
  { cmd_stream
  ; push_cmd
  ; last_identify = None
  ; token
  ; intents
  ; shards = []
  ; event_handler
  }
;;

let spawn ~token ~intents ~shards ~shard_count ~ws_url ~with_presence ~push_to_coordinator
  =
  let rec find_start_index max = function
    | (id, _, _) :: xs when id > max -> find_start_index id xs
    | _ :: xs -> find_start_index max xs
    | [] -> max + 1
  and wrap_push push c = push @@ Some c in
  let rec spawn' spawned_shards cnt next_shard_id =
    match cnt with
    | 0 -> return spawned_shards
    | cnt ->
      let chan, push = Lwt_stream.create () in
      let push = wrap_push push in
      let%lwt shard =
        Shard.init
          ~id:next_shard_id
          ~token
          ~intents
          ~push_to_coordinator
          ~ws_url
          ~with_presence
      in
      Lwt.async (fun () -> Runner.start ~cmd_stream:chan shard);
      spawn' ((next_shard_id, push, false) :: spawned_shards) (cnt - 1) (next_shard_id + 1)
  in
  shards
  |> find_start_index (-1) (* Starting at -1 so initial shard has index 0 *)
  |> spawn' [] shard_count
  >|= fun spawned ->
  List.append spawned shards |> List.sort (fun (a, _, _) (b, _, _) -> a - b)
;;

let run t =
  let open Runner_commands in
  let rec run' t =
    let t =
      (* check if 5 seconds have passed - okay to identify *)
      if t.last_identify /// 0. > Unix.time () -. 5.
      then t
      else (
        (* we can identify if necessary, glhf *)
        match List.find_opt (fun (_, _, identified) -> identified = false) t.shards with
        | Some (id, push, _) ->
          Logs.debug (fun m -> m "OK to identify shard %d." id);
          push (Identify (List.length t.shards));
          let shard = id, push, true in
          let shards =
            List.map
              (fun (sid, sp, si) -> if sid = id then shard else sid, sp, si)
              t.shards
          in
          { t with shards; last_identify = Some (Unix.time ()) }
        | None -> t)
    in
    match Lwt_stream.get_available_up_to 1 t.cmd_stream with
    | [] ->
      let%lwt _ = Lwt_unix.sleep 0.1 in
      run' t
    | [ cmd ] ->
      (match cmd with
       | Spawn (shard_count, ws_url, with_presence, res_pipe) ->
         Logs.debug (fun m -> m "Received request to spawn %d shards" shard_count);
         let%lwt shards =
           spawn
             ~token:t.token
             ~intents:t.intents
             ~shards:t.shards
             ~shard_count
             ~ws_url
             ~with_presence
             ~push_to_coordinator:t.push_cmd
         in
         let%lwt _ = Lwt_mvar.put res_pipe Ok in
         run' { t with shards }
       | Event event ->
         EventHandler.handle_event t.event_handler event;
         run' t
       | Shutdown shard_id ->
         let _, push, _ = t.shards |> List.find (fun (id, _, _) -> id = shard_id) in
         push Shutdown;
         Logs.debug (fun m -> m "Received shutdown for shard %d" shard_id);
         run' t
       | ShutdownAll ->
         Logs.debug (fun m -> m "Received shutdown for all shards");
         return ())
    | _ ->
      (* get_available_up_to limits this to a list of up to 1 element, but the type system doesn't know that so we need to have an arm for this *)
      failwith "unreachable"
  in
  Lwt.async (fun () -> run' t);
  t.push_cmd
;;

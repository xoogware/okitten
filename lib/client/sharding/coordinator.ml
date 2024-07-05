open Lwt
open Utils
open Commands.Coordinator

(** Represents a {!Shard} spawned by the Coordinator. ID * Push * Is Identified? *)
type tracked_shard = int * (Commands.Shard.command option -> unit) * bool

type t =
  { cmd_stream : command Lwt_stream.t
  ; push_cmd : command option -> unit
  ; last_identify : float option
  ; token : string
  ; shards :
      tracked_shard list (* TODO: use a map for this instead for faster replacements *)
  }

let init ~token =
  let cmd_stream, push_cmd = Lwt_stream.create () in
  { cmd_stream; push_cmd; last_identify = None; token; shards = [] }
;;

let spawn ~token ~shards ~shard_count ~push_to_coordinator =
  let rec find_start_index max = function
    | (id, _, _) :: xs when id > max -> find_start_index id xs
    | _ :: xs -> find_start_index max xs
    | [] -> max + 1
  in
  let rec spawn' spawned_shards cnt next_shard_id =
    match cnt with
    | 0 -> spawned_shards
    | cnt ->
      let chan, push = Lwt_stream.create () in
      let shard =
        Shard.init ~id:next_shard_id ~token ~intents:0 ~cmd:chan ~push_to_coordinator
      in
      Lwt.async (fun () -> Shard.start shard);
      spawn' ((next_shard_id, push, false) :: spawned_shards) (cnt - 1) (next_shard_id + 1)
  in
  shards
  |> find_start_index 0
  |> spawn' [] shard_count
  |> List.append shards
  |> List.sort (fun (a, _, _) (b, _, _) -> a - b)
;;

let run t =
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
          push (Some Identify);
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
       | Spawn (shard_count, res_pipe) ->
         Logs.debug (fun m -> m "Received request to spawn %d shards" shard_count);
         let shards =
           spawn
             ~token:t.token
             ~shards:t.shards
             ~shard_count
             ~push_to_coordinator:t.push_cmd
         in
         let%lwt _ = Lwt_mvar.put res_pipe Ok in
         Logs.debug (fun m -> m "hi");
         run' { t with shards }
       | Shutdown shard_id ->
         let _, push, _ = t.shards |> List.find (fun (id, _, _) -> id = shard_id) in
         push (Some Shutdown);
         Logs.debug (fun m -> m "Received shutdown for shard %d" shard_id);
         run' t
       | ShutdownAll ->
         Logs.debug (fun m -> m "Received shutdown for all shards");
         return ())
    | _ -> failwith "unreachable"
  in
  Lwt.async (fun () -> run' t);
  t.push_cmd
;;

open Lwt
open Commands.Coordinator

type t =
  { cmd_stream : command Lwt_stream.t
  ; push_cmd : command option -> unit
  ; last_identify : float option
  ; token : string
  }

let init ~token =
  let cmd_stream, push_cmd = Lwt_stream.create () in
  { cmd_stream; push_cmd; last_identify = None; token }
;;

let spawn ~token ~shard_channels ~shard_count ~push_to_coordinator =
  let rec find_start_index max = function
    | x :: xs when fst x > max -> find_start_index (fst x) xs
    | _ :: xs -> find_start_index max xs
    | [] -> max + 1
  in
  let rec spawn' spawned_shard_channels cnt next_shard_id =
    match cnt with
    | 0 -> spawned_shard_channels
    | cnt ->
      let chan, push = Lwt_stream.create () in
      let shard =
        Shard.init ~id:next_shard_id ~token ~intents:0 ~cmd:chan ~push_to_coordinator
      in
      Lwt.async (fun () -> Shard.start shard);
      spawn'
        ((next_shard_id, push) :: spawned_shard_channels)
        (cnt - 1)
        (next_shard_id + 1)
  in
  shard_channels
  |> find_start_index 0
  |> spawn' [] shard_count
  |> List.append shard_channels
  |> List.sort (fun a b -> fst a - fst b)
;;

let run t =
  let rec run' ~shard_channels t =
    match%lwt Lwt_stream.get t.cmd_stream with
    | None -> Lwt_unix.sleep 0.1 >>= fun () -> run' ~shard_channels t
    | Some cmd ->
      (match cmd with
       | Spawn (shard_count, res_pipe) ->
         let shard_channels =
           spawn
             ~token:t.token
             ~shard_channels
             ~shard_count
             ~push_to_coordinator:t.push_cmd
         in
         let%lwt _ = Lwt_mvar.put res_pipe Ok in
         run' ~shard_channels t
       | Shutdown shard_id ->
         let push = shard_channels |> List.find (fun s -> fst s = shard_id) |> snd in
         push (Some Shutdown);
         run' ~shard_channels t)
  in
  Lwt.async (fun () -> run' ~shard_channels:[] t);
  t.push_cmd
;;

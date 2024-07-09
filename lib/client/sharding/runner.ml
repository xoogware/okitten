open Lwt
open Commands.Shard

let do_actions ~cancellation_semaphore s =
  let rec perform s = function
    | [] -> return @@ Some s
    | Identify total_shards :: xs ->
      let%lwt s = Shard.send_identify ~total_shards s in
      Lwt.async (fun () -> Shard.handle_gateway_rx ~cancellation_semaphore s);
      perform s xs
    | Handle packet :: xs ->
      let%lwt s = Shard.handle_packet ~packet ~cancellation_semaphore s in
      perform s xs
    | SetPresence p :: xs ->
      let open Models.Gateway in
      let%lwt _ =
        p
        |> payload_of ~op:PresenceUpdate
        |> yojson_of_payload Presence.yojson_of_t
        |> Shard.send_payload s
      in
      perform s xs
    | Shutdown :: _ ->
      let%lwt _ = Lwt_mvar.put cancellation_semaphore () in
      return None
  in
  s |> Shard.cmds |> Lwt_stream.get_available |> perform s
;;

let start shard =
  let rec event_loop ~cancellation_semaphore s =
    s
    |> Shard.try_heartbeat
    >>= do_actions ~cancellation_semaphore
    >>= function
    | Some s ->
      let%lwt _ = Lwt_unix.sleep 0.1 in
      event_loop ~cancellation_semaphore s
    | None -> return ()
  in
  Logs.debug (fun m -> m "Starting shard with ID %d." @@ Shard.id shard);
  (* this will be filled with a unit value to signal cancellation to gateway listener *)
  let cancellation_semaphore = Lwt_mvar.create_empty () in
  event_loop ~cancellation_semaphore shard
;;

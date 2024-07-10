open Lwt

let do_actions ~push_packet ~cmd_stream ~cancellation_semaphore s =
  let open Runner_commands in
  let rec perform s = function
    | [] -> return s
    | Identify total_shards :: xs ->
      let%lwt s = Shard.send_identify ~total_shards s in
      Lwt.async (fun () -> Shard.listen ~push_packet ~cancellation_semaphore s);
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
    | GetShard (id, res) :: xs ->
      let%lwt _ = if Shard.id s = id then Lwt_mvar.put res s else return () in
      perform s xs
    | Shutdown :: _ ->
      let%lwt _ = Lwt_mvar.put cancellation_semaphore () in
      return s
  in
  cmd_stream |> Lwt_stream.get_available |> perform s
;;

let rec handle_packets ~cancellation_semaphore ~cmd_stream s =
  match cmd_stream with
  | [] -> return s
  | packet :: xs ->
    let%lwt s = Shard.handle_packet ~packet ~cancellation_semaphore s in
    handle_packets ~cancellation_semaphore ~cmd_stream:xs s
;;

let start ~cmd_stream shard =
  let packets, push_packet = Lwt_stream.create () in
  let rec event_loop ~cancellation_semaphore s =
    let%lwt s =
      s
      |> Shard.try_heartbeat ~cancellation_semaphore
      >>= do_actions ~push_packet ~cmd_stream ~cancellation_semaphore
      >>= handle_packets
            ~cancellation_semaphore
            ~cmd_stream:(Lwt_stream.get_available packets)
    in
    match Lwt_mvar.is_empty cancellation_semaphore with
    | true ->
      let%lwt _ = Lwt_unix.sleep 0.1 in
      event_loop ~cancellation_semaphore s
    | false -> return ()
  in
  Logs.debug (fun m -> m "Starting shard with ID %d." @@ Shard.id shard);
  (* this will be filled with a unit value to signal cancellation to gateway listener *)
  let cancellation_semaphore = Lwt_mvar.create_empty () in
  event_loop ~cancellation_semaphore shard
;;

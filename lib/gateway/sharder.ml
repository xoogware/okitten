open Lwt
open Websocket

module Shard = struct
  type state =
    { id : int * int
    ; is_ready : bool ref
    ; seq : int
    ; frame_stream : Frame.t Lwt_stream.t * (Frame.t option -> unit)
    ; conn : Websocket_lwt_unix.conn
    ; heartbeat_interval : float ref
    ; session_id : string option
    }

  type t =
    { state : state
    ; stopped : bool
    ; can_resume : bool
    }

  let identify = Lwt_mutex.create ()

  let parse_frame (frame : Frame.t) =
    let open Frame.Opcode in
    match frame.opcode with
    | Text -> `Ok (Yojson.Safe.from_string frame.content)
    | Binary -> `Error "decompression unimplemented"
    | Close -> `Close frame
    | op ->
      let op = Frame.Opcode.to_string op in
      `Error ("unexpected opcode " ^ op)
  ;;

  let send_json ~content shard =
    let%lwt _ = Websocket_lwt_unix.write shard.conn @@ Frame.create ~content () in
    return shard
  ;;

  let push_frame ?payload ~ev shard =
    let content =
      match payload with
      | None -> ""
      | Some p ->
        `Assoc [ "op", `Int (Events.Opcode.to_int ev); "d", p ] |> Yojson.Safe.to_string
    in
    Logs.debug (fun f -> f "Sending frame %s" content);
    send_json ~content shard
  ;;

  let heartbeat shard =
    match shard.seq with
    | 0 -> return shard
    | i ->
      ignore
      @@ Logs_lwt.debug (fun f ->
        f "Sending Heartbeat - Shard %d (%d), sequence %d" (fst shard.id) (snd shard.id) i);
      let open Events.Opcode in
      let payload =
        `Assoc [ "op", `Int (to_int Heartbeat); "d", `Int shard.seq ]
        |> Yojson.Safe.to_string
      in
      send_json ~content:payload shard
  ;;

  let identify_or_resume ?data shard =
    let module J = Yojson.Safe.Util in
    let shard =
      match data with
      | Some data ->
        Logs.info (fun f -> f "hi %s" @@ Yojson.Safe.to_string data);
        let intv = J.(member "heartbeat_interval" data |> to_int) in
        shard.heartbeat_interval := float_of_int intv /. 1000.;
        shard
      | None -> shard
    in
    let open Events in
    match shard.session_id with
    | None ->
      Lwt_mutex.with_lock identify (fun _ ->
        Logs.debug (fun f -> f "Identify shard %d (%d)" (fst shard.id) (snd shard.id));
        let payload =
          { token = !Client_options.token
          ; compress = false
          ; large_threshold = 50
          ; shard = shard.id
          ; intents = 0
          ; properties = { os = "linux"; browser = "okitten"; device = "okitten" }
          }
          |> Identify.to_yojson
        in
        push_frame ~payload ~ev:Opcode.Identify shard)
    | Some id ->
      let payload =
        { token = !Client_options.token; session_id = id; seq = shard.seq }
        |> Resume.to_yojson
      in
      push_frame ~payload ~ev:Opcode.Resume shard
  ;;

  let handle_frame ~frame shard =
    let open Events.Opcode in
    let module J = Yojson.Safe.Util in
    let op = J.(member "op" frame |> to_int) |> of_int in
    match op with
    | Dispatch ->
      Logs.debug (fun f -> f "Received dispatch");
      return shard
    | Heartbeat -> heartbeat shard
    | InvalidSession ->
      Logs.err (fun f ->
        f
          "Received invalid session on shard %d (%d)! %s"
          (fst shard.id)
          (snd shard.id)
          (Yojson.Safe.pretty_to_string frame));
      (* if d is true the shard should resume https://discord.com/developers/docs/topics/gateway#resuming *)
      (match J.(member "d" frame |> to_bool) with
       | true -> identify_or_resume shard
       | false -> identify_or_resume { shard with session_id = None })
    | Reconnect -> return shard
    | Hello -> identify_or_resume ~data:(J.member "d" frame) shard
    | HeartbeatAck -> return shard
    | opcode ->
      Logs.warn (fun f ->
        f
          "Received invalid opcode (%s) on shard %d (%d)"
          (Events.Opcode.to_string opcode)
          (fst shard.id)
          (snd shard.id));
      return shard
  ;;

  let create ~url id () =
    let uri =
      url ^ "?v=10&encoding=json"
      |> Str.replace_first (Str.regexp "wss") "https"
      |> Uri.of_string
    in
    let%lwt addy = Resolver_lwt.resolve_uri ~uri Resolver_lwt_unix.system in
    let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
    let%lwt client = Conduit_lwt_unix.endp_to_client ~ctx addy in
    let%lwt conn = Websocket_lwt_unix.connect ~ctx client uri in
    return
      { id
      ; is_ready = ref true
      ; seq = 0
      ; frame_stream = Lwt_stream.create ()
      ; conn
      ; heartbeat_interval = ref 0.
      ; session_id = None
      }
  ;;
end

type t = { shards : Shard.t list }

let start ?count () =
  let module J = Yojson.Safe.Util in
  let%lwt data = Rest.get_gateway_bot () in
  let data =
    match data with
    | Ok d -> d
    | Error e -> failwith e
  in
  let url = J.(member "url" data |> to_string) in
  let count =
    match count with
    | Some c -> c
    | None -> J.(member "shards" data |> to_int)
  in
  let rec listen (t : Shard.t) =
    let handle_frame (t : Shard.t) =
      let%lwt frame = Websocket_lwt_unix.read t.state.conn in
      match Shard.parse_frame frame with
      | `Ok frame ->
        Logs.debug (fun f ->
          f
            "Shard %d (%d) received frame %s"
            (fst t.state.id)
            (snd t.state.id)
            (Yojson.Safe.to_string frame));
        Shard.handle_frame ~frame t.state >>= fun s -> return { t with state = s }
      | `Close c ->
        Logs.warn (fun f ->
          f
            "Shard %d (%d) received close frame: %s"
            (fst t.state.id)
            (snd t.state.id)
            (Frame.show c));
        return t
      | `Error e ->
        Logs.warn (fun f ->
          f "Websocket error on shard %d (%d): %s" (fst t.state.id) (snd t.state.id) e);
        return t
    in
    match t.stopped with
    | true -> return ()
    | false -> handle_frame t >>= listen
  in
  (* TODO: remove this: suppress unused warning temporarily *)
  Logs.info (fun f -> f "Connecting at url %s" url);
  (* TODO: this is stolen from disml. rewrite to make more sense *)
  let rec make_shards ids shards =
    match ids with
    | id, total when id >= total -> return shards
    | id, total ->
      let wrap state = return Shard.{ state; stopped = false; can_resume = true } in
      let run (shard : Shard.t) =
        let rec hb_loop () =
          Logs.debug (fun f ->
            f
              "Waiting %f sec to heartbeat on shard %d"
              !(shard.state.heartbeat_interval)
              (fst shard.state.id));
          let%lwt _ = Lwt_unix.sleep !(shard.state.heartbeat_interval) in
          let%lwt _ = Shard.heartbeat shard.state in
          hb_loop ()
        in
        Lwt.async hb_loop;
        Lwt.async (fun () -> listen shard);
        return shard
      in
      let make () = Shard.create ids ~url () in
      make ()
      >>= wrap
      >>= run
      >>= fun shard -> make_shards (id + 1, total) (shard :: shards)
  in
  make_shards (0, count) [] >|= fun shards -> { shards }
;;

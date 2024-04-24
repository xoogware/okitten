open Lwt
open Events
open Websocket

module Shard = struct
  type state =
    { id : int * int
    ; is_ready : bool ref
    ; seq : int
    ; frame_stream : Frame.t Lwt_stream.t * (Frame.t option -> unit)
    ; conn : Websocket_lwt_unix.conn
    ; heartbeat_interval : int
    }

  type t =
    { state : state
    ; stopped : bool
    ; can_resume : bool
    }

  let identify = Mutex.create ()
  let _ = Mutex.lock identify

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

  let push_frame ?payload ~ev shard =
    let content =
      match payload with
      | None -> ""
      | Some p ->
        Yojson.Safe.to_string @@ `Assoc [ "op", `Int (Events.to_int ev); "d", p ]
    in
    Websocket_lwt_unix.write shard.conn @@ Frame.create ~content ()
    >>= fun () -> return shard
  ;;

  let heartbeat shard =
    match shard.seq with
    | 0 -> return shard
    | i ->
      ignore
      @@ Logs_lwt.debug (fun f ->
        f "Sending Heartbeat - Shard %d (%d), sequence %d" (fst shard.id) (snd shard.id) i);
      push_frame ~payload:(`Int i) ~ev:Heartbeat shard
  ;;

  let initialize ?data shard =
    let module J = Yojson.Safe.Util in
    let shard =
      match data with
      | Some data ->
        Ok
          { shard with
            heartbeat_interval = J.(member "heartbeat_interval" data |> to_int)
          }
      | None -> Error "Failed to establish heartbeat"
    in
    shard
  ;;

  let handle_frame ~frame shard =
    let module J = Yojson.Safe.Util in
    let op = J.(member "op" frame |> to_int) |> Events.of_int in
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
       | true -> return shard
       | false -> return shard)
    | Reconnect -> return shard
    | Hello -> return shard
    | HeartbeatAck -> return shard
    | opcode ->
      Logs.warn (fun f ->
        f
          "Received invalid opcode (%s) on shard %d (%d)"
          (Events.to_string opcode)
          (fst shard.id)
          (snd shard.id));
      return shard
  ;;

  let create id () =
    let uri = "wss://gateway.discord.gg/?v=10&encoding=json" |> Uri.of_string in
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
      ; heartbeat_interval = 0
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
  let shard_list = 0, count in
  let rec listen (t : Shard.t) =
    let handle_frame (t : Shard.t) =
      let%lwt frame = Websocket_lwt_unix.read t.state.conn in
      match Shard.parse_frame frame with
      | `Ok f ->
        Shard.handle_frame ~frame:f t.state >>= fun s -> return { t with state = s }
      | `Close c ->
        Logs.warn (fun f ->
          f
            "Shard %d (%d) received close frame: %s"
            (fst t.state.id)
            (snd t.state.id)
            (Frame.show c));
        return t
    in
    match t.stopped with
    | true -> return ()
    | false -> handle_frame t >>= listen
  in
  Logs.info (fun f -> f "Connecting at url %s" url);
  return ()
;;

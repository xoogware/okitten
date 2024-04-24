type 'a frame =
  { op : int
  ; d : 'a
  ; s : int option
  ; t : string option
  }
[@@deriving yojson]

module type EventCollection = sig
  type t [@@deriving yojson]
end

module SendEvent : EventCollection = struct
  type t =
    | Identify of Identify.t
    | Resume
    | Heartbeat
    | RequestGuildMembers
    | UpdateVoiceState
    | UpdatePresence
  [@@deriving yojson]

  let to_int = function
    | Heartbeat -> 1
    | Identify _ -> 2
    | UpdatePresence -> 3
    | UpdateVoiceState -> 4
    | Resume -> 6
    | RequestGuildMembers -> 8
  ;;

  let make_frame d =
    let op = to_int d in
    { op; d; s = None; t = None }
  ;;
end

module GatewayEvent : EventCollection = struct
  type t =
    | Dispatch
    | Heartbeat
    | Reconnect
    | InvalidSession
    | Hello
    | HeartbeatAck
  [@@deriving yojson]

  let of_frame frame =
    match frame.d with
    | 0 -> Dispatch
    | 1 -> Heartbeat
    | 7 -> Reconnect
    | 9 -> InvalidSession
    | 10 -> Hello
  ;;
end

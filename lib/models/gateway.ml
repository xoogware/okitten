type session_start_limit =
  { total : int
  ; remaining : int
  ; reset_after : int
  ; max_concurrency : int
  }
[@@deriving yojson]

type bot_gateway =
  { url : string
  ; shards : int
  ; session_start_limit : session_start_limit
  }
[@@deriving yojson]

type identify_connection =
  { os : string
  ; browser : string
  ; device : string
  }
[@@deriving yojson]

type identify =
  { token : string
  ; properties : identify_connection
  ; compress : bool
  ; large_threshold : int
  ; shard : int * int
  ; intents : int
  }
[@@deriving yojson]

type hello = { heartbeat_interval : int }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type heartbeat = int option [@@deriving yojson]

module Dispatch = Dispatch

module Opcode = struct
  type t =
    | Dispatch
    | Heartbeat
    | Identify
    | PresenceUpdate
    | Resume
    | Reconnect
    | RequestGuildMembers
    | InvalidSession
    | Hello
    | HeartbeatAck

  let yojson_of_t = function
    | Dispatch -> `Int 0
    | Heartbeat -> `Int 1
    | Identify -> `Int 2
    | PresenceUpdate -> `Int 3
    | Resume -> `Int 6
    | Reconnect -> `Int 7
    | RequestGuildMembers -> `Int 8
    | InvalidSession -> `Int 9
    | Hello -> `Int 10
    | HeartbeatAck -> `Int 11
  ;;

  let of_int = function
    | 0 -> Dispatch
    | 1 -> Heartbeat
    | 2 -> Identify
    | 3 -> PresenceUpdate
    | 6 -> Resume
    | 7 -> Reconnect
    | 8 -> RequestGuildMembers
    | 9 -> InvalidSession
    | 10 -> Hello
    | 11 -> HeartbeatAck
    | _ -> failwith "Invalid opcode"
  ;;

  let t_of_yojson = function
    | `Int c -> of_int c
    | _ -> failwith "Invalid opcode"
  ;;

  let to_string = function
    | Dispatch -> "Dispatch"
    | Heartbeat -> "Heartbeat"
    | Identify -> "Identify"
    | PresenceUpdate -> "PresenceUpdate"
    | Resume -> "Resume"
    | Reconnect -> "Reconnect"
    | RequestGuildMembers -> "RequestGuildMembers"
    | InvalidSession -> "InvalidSession"
    | Hello -> "Hello"
    | HeartbeatAck -> "HeartbeatAck"
  ;;
end

type 'a payload =
  { op : Opcode.t
  ; d : 'a
  ; t : int option
  ; s : int option
  }
[@@deriving yojson]

let payload_of ~op ?s d = { op; d; t = None; s }

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

type 'a payload =
  { op : int
  ; d : 'a
  }
[@@deriving yojson]

let payload_of ~op d =
  let op =
    match op with
    | `Dispatch -> 0
    | `Heartbeat -> 1
    | `Identify -> 2
    | `PresenceUpdate -> 3
    | `Resume -> 6
    | `Reconnect -> 7
    | `RequestGuildMembers -> 8
    | `InvalidSession -> 9
    | `Hello -> 10
    | `HeartbeatAck -> 11
  in
  { op; d }
;;

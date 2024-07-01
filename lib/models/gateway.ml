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

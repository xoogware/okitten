type t =
  { token : string
  ; session_id : string
  ; seq : int
  }
[@@deriving yojson]

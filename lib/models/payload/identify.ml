type properties =
  { os : string
  ; browser : string
  ; device : string
  }
[@@deriving yojson]

type t =
  { token : string
  ; properties : properties
  ; compress : bool
  ; large_threshold : int
  ; shard : int * int
  ; intents : int
  }
[@@deriving yojson]

type conn_properties =
  { os : string
  ; browser : string
  ; device : string
  }
[@@deriving yojson]

type t =
  { token : string
  ; compress : bool
  ; large_threshold : int
  ; shard : int * int
  ; intents : int
  ; properties : conn_properties
  }
[@@deriving yojson]

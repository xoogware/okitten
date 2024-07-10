type avatar_decoration_data =
  { asset : string
  ; sku_id : string
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type t =
  { id : string
  ; username : string
  ; discriminator : string
  ; global_name : string
  ; avatar : string
  ; bot : bool option [@yojson.option]
  ; system : bool option [@yojson.option]
  ; mfa_enabled : bool option [@yojson.option]
  ; banner : string option [@yojson.option]
  ; accent_color : string option [@yojson.option]
  ; locale : string option [@yojson.option]
  ; verified : bool option [@yojson.option]
  ; email : string option [@yojson.option]
  ; flags : int option [@yojson.option]
  ; premium_type : int option [@yojson.option]
  ; public_flags : int option [@yojson.option]
  ; avatar_decoration_data : avatar_decoration_data option
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

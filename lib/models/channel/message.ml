type t =
  { id : string
  ; channel_id : string
  ; author : User.t
  ; content : string
  ; timestamp : Models.date
  ; edited_timestamp : Models.date option
  ; tts : bool
  ; mention_everyone : bool
  ; mentions : User.t list
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

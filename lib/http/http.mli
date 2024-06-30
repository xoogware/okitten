type t =
  { application_id : string option
  ; ratelimiter : int option
  ; token : string
  }

module Builder : sig
  type builder =
    { application_id : string option
    ; ratelimiter : int option
    ; token : string
    }

  val create : token:string -> builder
  val set_application_id : string -> builder -> builder
  val set_token : string -> builder -> builder
  val set_ratelimiter : int -> builder -> builder
  val build : builder -> t
end

type http_method =
  | Delete
  | Get
  | Patch
  | Post
  | Put

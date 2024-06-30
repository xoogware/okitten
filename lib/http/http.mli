type t =
  { application_id : string option
  ; ratelimiter : Ratelimiter.t
  ; token : string
  }

module Builder : sig
  type builder =
    { application_id : string option
    ; token : string
    }

  val create : token:string -> builder
  val set_application_id : string -> builder -> builder
  val set_token : string -> builder -> builder
  val build : builder -> t
end

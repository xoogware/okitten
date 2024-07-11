type t

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

val fire : Request.t -> http:t -> Ratelimiter.response Lwt.t
val listen : t -> unit Lwt.t
val get_bot_gateway : t -> Models.Gateway.bot_gateway Lwt.t

(** The Ratelimiter. *)
type t

(** Details for a ratelimit incident. *)
type ratelimit_info =
  { limit : int
  ; timeout_after : float
  ; http_method : Cohttp.Code.meth
  ; path : string
  ; global : bool
  }

(** The type that the Ratelimiter will return to the caller. *)
type response = Cohttp_lwt.Response.t * Cohttp_lwt.Body.t

(** Creates a new ratelimiter with the given token. *)
val init : token:string -> t

(** Sets the callback called upon being ratelimited. *)
val set_callback : (ratelimit_info -> unit) -> t -> t

(** Whether to use absolute ratelimits.
    If true, ratelimit expiry is taken from X-RateLimit-Reset.
    Otherwise, it's calculated using system time plus X-RateLimit-Reset-After. *)
val use_absolute_ratelimits : bool -> t -> t

(** Begins the ratelimiter event loop. *)
val watch_requests : t -> unit Lwt.t

(** Queues a {!Request.t} for execution. Response will be filled to the mvar passed. *)
val enqueue : request:Request.t -> response:response Lwt_mvar.t -> t -> unit

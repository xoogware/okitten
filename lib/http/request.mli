type meth =
  | Delete
  | Get
  | Patch
  | Post
  | Put

(** Represents a request to make through the {!Ratelimiter}. *)
type t =
  { body : string option
  ; headers : Cohttp.Header.t option
  ; meth : meth
  ; params : string list option
  ; route : string
  }

(** Creates a {!Request.t} with the given route and method. *)
val make : route:string -> meth:meth -> t

(** Sets the body of a {!Request.t}. *)
val body : string option -> t -> t

(** Sets the headers of a {!Request.t}. *)
val headers : Cohttp.Header.t option -> t -> t

(** Sets the URL parameters of a {!Request.t}. *)
val params : string list option -> t -> t

val get_method : t -> Cohttp.Code.meth

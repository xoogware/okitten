module RouteMap : Map.S with type key = string

type ratelimit =
  { limit : int
  ; remaining : int
  ; reset : int
  }

type t = ratelimit Lwt_mvar.t RouteMap.t

val of_headers : Cohttp.Header.t -> ratelimit option
val empty : t

val update
  :  string
  -> (ratelimit Lwt_mvar.t option -> ratelimit Lwt_mvar.t option)
  -> t
  -> t

val find : string -> t -> ratelimit Lwt_mvar.t
val find_opt : string -> t -> ratelimit Lwt_mvar.t option
val get : path:string -> t -> ratelimit Lwt_mvar.t * t

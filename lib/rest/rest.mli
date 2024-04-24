type 'a res = ('a, string) result Lwt.t

val get_gateway : unit -> Yojson.Safe.t res
val get_gateway_bot : unit -> Yojson.Safe.t res

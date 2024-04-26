module RouteMap = Map.Make (String)

type ratelimit =
  { limit : int
  ; remaining : int
  ; reset : float
  }

type t = ratelimit Lwt_mvar.t RouteMap.t

let of_headers h =
  let module H = Cohttp.Header in
  match
    ( H.get h "X-RateLimit-Limit"
    , H.get h "X-RateLimit-Remaining"
    , H.get h "X-RateLimit-Reset" )
  with
  | Some l, Some r, Some t ->
    let limit = int_of_string l in
    let remaining = int_of_string r in
    let reset = float_of_string t in
    Some { limit; remaining; reset }
  | _ -> None
;;

let empty = RouteMap.empty
let update = RouteMap.update
let find = RouteMap.find
let find_opt = RouteMap.find_opt

let get ~path ratelimit =
  match find_opt path ratelimit with
  | Some r -> r, ratelimit
  | None ->
    let data = Lwt_mvar.create { limit = 1; remaining = 1; reset = 0. } in
    let ratelimit = RouteMap.add path data ratelimit in
    data, ratelimit
;;

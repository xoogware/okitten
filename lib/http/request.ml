type meth =
  | Delete
  | Get
  | Patch
  | Post
  | Put

type t =
  { body : string option
  ; headers : Cohttp.Header.t option
  ; meth : meth
  ; params : string list option
  ; route : string
  }

let make ~route ~meth = { body = None; headers = None; meth; params = None; route }
let body body r = { r with body }
let headers headers r = { r with headers }
let params params r = { r with params }

let get_method r =
  match r.meth with
  | Delete -> `DELETE
  | Get -> `GET
  | Patch -> `PATCH
  | Post -> `POST
  | Put -> `PUT
;;

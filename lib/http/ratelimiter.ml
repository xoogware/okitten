open Lwt
module RouteMap = Map.Make (String)

type ratelimit =
  { limit : int
  ; remaining : int
  ; reset_at : float
  ; reset_after : float
  }

type ratelimit_info =
  { limit : int
  ; timeout_after : float
  ; http_method : Http.http_method
  ; path : string
  ; global : bool
  }

type request = Cohttp.Code.meth * string * Cohttp.Header.t * Cohttp_lwt.Body.t option
type request_queue_item = request * (Cohttp_lwt.Response.t * Cohttp_lwt.Body.t) Lwt_mvar.t

type t =
  { routes : ratelimit RouteMap.t
  ; request_queue : request_queue_item Lwt_stream.t
  ; push : request_queue_item option -> unit
  ; token : string
  ; callback : ratelimit_info -> unit
  ; use_absolute_ratelimits : bool
  }

let init token =
  let request_queue, push = Lwt_stream.create () in
  { routes = RouteMap.empty
  ; request_queue
  ; push
  ; token
  ; callback = (fun _ -> ())
  ; use_absolute_ratelimits = false
  }
;;

let set_callback self callback = { self with callback }
let use_absolute_ratelimits self r = { self with use_absolute_ratelimits = r }

let rec watch_requests ratelimiter =
  let get_or_default route ratelimits =
    match RouteMap.find_opt route ratelimits with
    | Some r -> r
    | None -> { limit = 0; remaining = 0; reset_at = 0.; reset_after = 0. }
  in
  let increment_ratelimit resource (response : Cohttp_lwt.Response.t) self =
    let limit = Cohttp.Header.get response.headers "X-RateLimit-Limit" in
    let remaining = Cohttp.Header.get response.headers "X-RateLimit-Remaining" in
    let reset = Cohttp.Header.get response.headers "X-RateLimit-Reset" in
    let reset_after = Cohttp.Header.get response.headers "X-RateLimit-Reset-After" in
    let bucket = Cohttp.Header.get response.headers "X-RateLimit-Bucket" in
    let updater = function
      | None -> Some { limit = 0; remaining = 0; reset_at = 0.; reset_after = 0. }
      | Some _ -> Some { limit = 0; remaining = 0; reset_at = 0.; reset_after = 0. }
    in
    let routes = RouteMap.update resource updater self.routes in
    { self with routes }
  in
  let perform ~meth ~resource ~headers ~body self =
    let uri = Uri.of_string resource in
    let%lwt response, body = Cohttp_lwt_unix.Client.call ~headers ?body meth uri in
    let self = increment_ratelimit resource response self in
    return (response, body, self)
  in
  let ratelimit_precheck now route =
    return
    @@ if route.reset_at < now then { route with remaining = route.limit } else route
  in
  match%lwt Lwt_stream.get ratelimiter.request_queue with
  | Some (request, response_channel) ->
    let now = Unix.time () in
    let meth, resource, headers, body = request in
    let%lwt route =
      get_or_default resource ratelimiter.routes |> ratelimit_precheck now
    in
    let ratelimiter =
      { ratelimiter with
        routes = RouteMap.update resource (fun _ -> Some route) ratelimiter.routes
      }
    in
    let%lwt response, body, ratelimiter =
      perform ~meth ~resource ~headers ~body ratelimiter
    in
    let%lwt _ = Lwt_mvar.put response_channel (response, body) in
    watch_requests ratelimiter
  | None ->
    let%lwt _ = Lwt_unix.sleep 0.1 in
    watch_requests ratelimiter
;;

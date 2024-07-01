open Lwt
module RouteMap = Map.Make (String)

type ratelimit_info =
  { limit : int
  ; timeout_after : float
  ; http_method : Cohttp.Code.meth
  ; path : string
  ; global : bool
  }

module Ratelimit = struct
  type t =
    { limit : int
    ; remaining : int
    ; reset_at : float option
    ; reset_after : float option
    }

  let preprocess_hook ~req ~cb rl =
    match rl.limit with
    | 0 -> return rl
    | limit ->
      (match rl.reset_at with
       | None -> return { rl with remaining = limit }
       | Some reset_at ->
         (match reset_at -. Unix.time () with
          | delay when delay <= 0. ->
            return
            @@ if rl.remaining = 0 then rl else { rl with remaining = rl.remaining - 1 }
          | delay ->
            (match rl.remaining with
             | 0 ->
               let open Request in
               Logs.debug (fun m ->
                 m
                   "Ratelimit reached on %s, preemptively blocking for %fms."
                   req.route
                   delay);
               cb
                 { limit = rl.limit
                 ; timeout_after = delay
                 ; http_method = get_method req
                 ; path = req.route
                 ; global = false
                 };
               let%lwt _ = Lwt_unix.sleep (delay /. 1000.) in
               return rl
             | _ -> return { rl with remaining = rl.remaining - 1 })))
  ;;

  let postprocess_hook ~headers rl =
    let open Cohttp.Header in
    let limit =
      match get headers "X-RateLimit-Limit" with
      | Some l -> int_of_string l
      | None -> rl.limit
    in
    let remaining =
      match get headers "X-RateLimit-Remaining" with
      | Some r -> int_of_string r
      | None -> rl.remaining
    in
    let reset_at =
      match get headers "X-RateLimit-Reset" with
      | Some r -> Some (float_of_string r)
      | None -> rl.reset_at
    in
    let reset_after =
      match get headers "X-RateLimit-Reset-After" with
      | Some r -> Some (float_of_string r)
      | None -> rl.reset_after
    in
    { limit; remaining; reset_at; reset_after }
  ;;
end

type response = Cohttp_lwt.Response.t * Cohttp_lwt.Body.t
type request_queue_item = Request.t * response Lwt_mvar.t

type t =
  { routes : Ratelimit.t RouteMap.t
  ; request_queue : request_queue_item Lwt_stream.t
  ; push : request_queue_item option -> unit
  ; token : string
  ; callback : ratelimit_info -> unit
  }

let init ~token =
  let request_queue, push = Lwt_stream.create () in
  { routes = RouteMap.empty; request_queue; push; token; callback = (fun _ -> ()) }
;;

let set_callback callback self = { self with callback }

let get_or_default route ratelimits =
  match RouteMap.find_opt route ratelimits with
  | Some r -> r
  | None -> Ratelimit.{ limit = 0; remaining = 0; reset_at = None; reset_after = None }
;;

let apply_preprocess ~req rl =
  let%lwt route =
    rl.routes
    |> get_or_default req.route
    |> Ratelimit.preprocess_hook ~req ~cb:rl.callback
  in
  return { rl with routes = RouteMap.update req.route (fun _ -> Some route) rl.routes }
;;

let apply_postprocess ~headers ~resource rl =
  let route =
    rl.routes |> get_or_default resource |> Ratelimit.postprocess_hook ~headers
  in
  { rl with routes = RouteMap.update resource (fun _ -> Some route) rl.routes }
;;

let rec watch_requests ratelimiter =
  let perform ~meth ~resource ~headers ~body self =
    let uri = Uri.of_string resource in
    let body = Option.map (fun b -> Cohttp_lwt.Body.of_string b) body in
    let headers = headers |> Utils.unwrap_or ~default:(Cohttp.Header.init ()) in
    let headers =
      Cohttp.Header.add_list
        headers
        [ "Authorization", self.token
        ; "User-Agent", "Discordbot (https://github.com/xoogware/okitten, 0.1)"
        ]
    in
    let%lwt response, body = Cohttp_lwt_unix.Client.call ~headers ?body meth uri in
    return (response, body, self)
  in
  match%lwt Lwt_stream.get ratelimiter.request_queue with
  | Some (r, response_channel) ->
    let%lwt ratelimiter = apply_preprocess ~req:r ratelimiter in
    let%lwt response, body, ratelimiter =
      perform
        ~meth:(Request.get_method r)
        ~resource:r.route
        ~headers:r.headers
        ~body:r.body
        ratelimiter
    in
    let ratelimiter =
      apply_postprocess ~headers:response.headers ~resource:r.route ratelimiter
    in
    let%lwt _ = Lwt_mvar.put response_channel (response, body) in
    watch_requests ratelimiter
  | None ->
    let%lwt _ = Lwt_unix.sleep 0.1 in
    watch_requests ratelimiter
;;

let enqueue ~request ~response rl = rl.push @@ Some (request, response)

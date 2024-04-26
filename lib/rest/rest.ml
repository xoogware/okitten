open Lwt

type 'a res = ('a, string) result Lwt.t

module Client = struct
  open Cohttp

  let ratelimits = ref Ratelimit.empty
  let base_url = "https://discord.com/api/v10"
  let uri_of path = Uri.of_string @@ base_url ^ path

  let get_ua () =
    "DiscordBot (https://github.com/bigspeedfpv/okitten," ^ Version.get () ^ ")"
  ;;

  let gen_headers () =
    let headers = Header.init () in
    Header.add_list
      headers
      [ "User-Agent", get_ua ()
      ; "Authorization", "Bot " ^ !Client_options.token
      ; "Content-Type", "application/json"
      ; "Connection", "keep-alive"
      ]
  ;;

  let serialize_req body = body |> Yojson.Safe.to_string |> Cohttp_lwt.Body.of_string

  let deserialize_res ~path (res, body) =
    let limit =
      match Response.headers res |> Ratelimit.of_headers with
      | Some r -> r
      | None -> { limit = 1; remaining = 1; reset = 0. }
    in
    Lwt_mvar.put (Ratelimit.find path !ratelimits) limit
    >>= fun () ->
    match res |> Response.status |> Code.code_of_status with
    | 204 -> return @@ Ok `Null
    | code when Code.is_success code ->
      let%lwt body = body |> Cohttp_lwt.Body.to_string >|= Yojson.Safe.from_string in
      return @@ Ok body
    | code ->
      let%lwt body = body |> Cohttp_lwt.Body.to_string in
      let headers = Response.sexp_of_t res |> Sexplib.Sexp.to_string_hum in
      Logs.warn (fun f ->
        f "Bad response! Code: %d\nheaders: %s\nbody: %s" code headers body);
      return @@ Error "Unsuccessful response received"
  ;;

  type request =
    | Delete
    | Get
    | Patch
    | Post
    | Put

  let request ?(body = `Null) ?(query = []) m p =
    let limit, updated = Ratelimit.get ~path:p !ratelimits in
    (* insert path if not exists *)
    ratelimits := updated;
    let%lwt limit = Lwt_mvar.take limit in
    let handle () =
      let uri = Uri.add_query_params (uri_of p) query in
      let headers = gen_headers () in
      let body = serialize_req body in
      (match m with
       | Delete -> Cohttp_lwt_unix.Client.delete ~headers ~body uri
       | Get -> Cohttp_lwt_unix.Client.get ~headers uri
       | Patch -> Cohttp_lwt_unix.Client.patch ~headers ~body uri
       | Post -> Cohttp_lwt_unix.Client.post ~headers ~body uri
       | Put -> Cohttp_lwt_unix.Client.put ~headers ~body uri)
      >>= deserialize_res ~path:p
    in
    match limit.remaining with
    | 0 ->
      (* sleep until the reset time has past - floor current time for extra headroom *)
      let run_after = limit.reset -. Unix.time () in
      Logs.debug (fun f -> f "RATELIMITED [%s] - running in %f ms" p run_after);
      Lwt_unix.sleep run_after >>= handle
    | _ -> handle ()
  ;;
end

let get_gateway () = Client.request Get Endpoints.gateway
let get_gateway_bot () = Client.request Get Endpoints.gateway_bot

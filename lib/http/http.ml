open Lwt

type t =
  { _application_id : string option
  ; ratelimiter : Ratelimiter.t
  }

let listen http = Ratelimiter.watch_requests http.ratelimiter

module Builder = struct
  type builder =
    { application_id : string option
    ; token : string
    }

  let parse_token t =
    try
      match String.contains t ' ' with
      | true -> t
      | false -> "Bot " ^ t
    with
    | _ -> t
  ;;

  let create ~token = { application_id = None; token = parse_token token }
  let set_application_id id b = { b with application_id = Some id }
  let set_token token b = { b with token = parse_token token }

  let build b =
    let http =
      { _application_id = b.application_id
      ; ratelimiter = Ratelimiter.init ~token:b.token
      }
    in
    Lwt.async (fun () -> listen http);
    http
  ;;
end

let fire request ~http =
  let response = Lwt_mvar.create_empty () in
  Ratelimiter.enqueue ~request ~response http.ratelimiter;
  Lwt_mvar.take response
;;

let get_bot_gateway http =
  let%lwt res = Request.make ~route:"/gateway/bot" ~meth:Get |> fire ~http in
  let%lwt str_body = res |> snd |> Cohttp_lwt.Body.to_string in
  return (str_body |> Yojson.Safe.from_string |> Models.Gateway.bot_gateway_of_yojson)
;;

open Lwt

type t =
  { application_id : string option
  ; ratelimiter : Ratelimiter.t
  }

module Builder = struct
  type builder =
    { application_id : string option
    ; token : string
    }

  let create ~token = { application_id = None; token }
  let set_application_id id b = { b with application_id = Some id }
  let set_token token b = { b with token }

  let build b =
    { application_id = b.application_id; ratelimiter = Ratelimiter.init ~token:b.token }
  ;;
end

let fire request ~http =
  let response = Lwt_mvar.create_empty () in
  Logs.debug (fun m -> m "%s" @@ Utils.unwrap_or ~default:"hi" http.application_id);
  Ratelimiter.enqueue ~request ~response http.ratelimiter;
  Lwt_mvar.take response
;;

let get_bot_gateway http =
  let%lwt res =
    Request.make ~route:"https://discord.com/api/v10/gateway/bot" ~meth:Get |> fire ~http
  in
  let%lwt str_body = res |> snd |> Cohttp_lwt.Body.to_string in
  return
  @@
  match str_body |> Yojson.Safe.from_string |> Models.Gateway.bot_gateway_of_yojson with
  | Ok b -> Ok b
  | Error e -> Error ("Unable to deserialize: " ^ e)
;;

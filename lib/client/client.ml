open Lwt

type t =
  { http : Http.t
  ; intents : int
  ; gateway : string
  }

module ClientBuilder = struct
  type t =
    { token : string
    ; intents : int
    }

  let init ~token = { token; intents = 0 }
  let set_intents intents builder = { builder with intents }

  let build b =
    let http = Http.Builder.create ~token:b.token |> Http.Builder.build in
    let%lwt gateway =
      match%lwt Http.get_bot_gateway http with
      | Ok g -> return g.url
      | Error e -> failwith e
    in
    return { http; intents = b.intents; gateway }
  ;;
end

let start c = return @@ Logs.info (fun m -> m "Connecting to gateway at %s" c.gateway)

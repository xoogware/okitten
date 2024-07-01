open Lwt

module ClientBuilder = struct
  type t =
    { http : Http.t
    ; intents : int
    ; gateway : string
    }

  let init ~token =
    let http = Http.Builder.(create ~token |> build) in
    let%lwt gateway =
      match%lwt Http.get_bot_gateway http with
      | Ok g -> return g.url
      | Error e -> failwith e
    in
    return { http; intents = 0; gateway }
  ;;
end

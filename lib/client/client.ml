module ClientBuilder = struct
  type t =
    { http : Http.t
    ; intents : int
    }

  let init ~token =
    let http = Http.Builder.(create ~token |> build) in
    { http; intents = 0 }
  ;;
end

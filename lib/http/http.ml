type t =
  { application_id : string option
  ; ratelimiter : int option
  ; token : string
  }

module Builder = struct
  type builder =
    { application_id : string option
    ; ratelimiter : int option
    ; token : string
    }

  let create ~token = { application_id = None; ratelimiter = None; token }
  let set_application_id id b = { b with application_id = Some id }
  let set_token token b = { b with token }
  let set_ratelimiter rl b = { b with ratelimiter = Some rl }

  let build b : t =
    { application_id = b.application_id; ratelimiter = b.ratelimiter; token = b.token }
  ;;
end

type http_method =
  | Delete
  | Get
  | Patch
  | Post
  | Put

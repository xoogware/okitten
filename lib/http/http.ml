type t =
  { application_id : string option
  ; ratelimiter : Ratelimiter.t
  ; token : string
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
    { application_id = b.application_id
    ; ratelimiter = Ratelimiter.init ~token:b.token
    ; token = b.token
    }
  ;;
end

let fire request http =
  let response = Lwt_mvar.create_empty () in
  Ratelimiter.enqueue ~request ~response http.ratelimiter;
  Lwt_mvar.take response
;;

open Lwt

type ctx = { shard_id : int }

type t =
  { on_ready : unit -> unit Lwt.t
  ; on_message : ctx -> Message.t -> unit Lwt.t
  }

type event =
  | Ready
  | MessageCreate of Message.t * int

let init () = { on_ready = (fun () -> return ()); on_message = (fun _ _ -> return ()) }
let set_on_ready f handler = { handler with on_ready = f }
let set_on_message f handler = { handler with on_message = f }

let handle_event handler event =
  Lwt.async (fun () ->
    match event with
    | Ready -> handler.on_ready ()
    | MessageCreate (msg, shard_id) -> handler.on_message { shard_id } msg)
;;

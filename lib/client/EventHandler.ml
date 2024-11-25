open Lwt

type t =
  { on_ready : unit -> unit Lwt.t
  ; on_message : Ctx.event_ctx -> Message.t -> unit Lwt.t
  }

type event =
  | Ready
  | MessageCreate of Message.t * int

let init () = { on_ready = (fun () -> return ()); on_message = (fun _ _ -> return ()) }
let set_on_ready f handler = { handler with on_ready = f }
let set_on_message f handler = { handler with on_message = f }

let apply_opt opt f builder =
  match opt with
  | Some v -> f v builder
  | None -> builder
;;

let init_with ?on_ready ?on_message () =
  init () |> apply_opt on_ready set_on_ready |> apply_opt on_message set_on_message
;;

let handle_event handler http event =
  Lwt.async (fun () ->
    match event with
    | Ready -> handler.on_ready ()
    | MessageCreate (msg, shard_id) -> handler.on_message { shard_id; http } msg)
;;

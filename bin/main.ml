open Okitten
open Lwt
open Lwt.Infix

let setup_logs () =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Fmt_tty.setup_std_outputs ()
;;

let rec wait () = Lwt_unix.sleep 5. >>= fun _ -> wait ()
let log_ready () = return @@ Logs.info (fun m -> m "Ready!")

let log_message _ctx (msg : Okitten.Models.Message.t) =
  return @@ Logs.info (fun m -> m "Got message: %s" msg.content)
;;

let () =
  let token =
    match Sys.getenv_opt "TOKEN" with
    | Some t -> t
    | None -> failwith "TOKEN not found in environment"
  in
  let main =
    setup_logs ();
    let activity =
      Presence.Activity.(
        empty
        |> set_name "OCaml evangelists"
        |> set_kind Listening
        |> set_state @@ Some "mreow ^_^")
    in
    let presence =
      Presence.(
        empty |> since_now |> with_activity activity |> set_status Idle |> set_afk false)
    in
    let event_handler =
      EventHandler.(init () |> set_on_ready log_ready |> set_on_message log_message)
    in
    let%lwt _ =
      ClientBuilder.(
        init
        |> set_token token
        |> set_intents Intents.(message_content lor guild_messages)
        |> set_event_handler event_handler
        |> build)
      >>= Client.start ~shards:`Autosharded ~with_presence:presence
    in
    wait ()
  in
  Lwt_main.run main
;;

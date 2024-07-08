open Okitten
open Lwt.Infix

let setup_logs () =
  Logs.set_level @@ Some Logs.Debug;
  Logs.set_reporter @@ Logs_fmt.reporter ();
  Fmt_tty.setup_std_outputs ()
;;

let rec wait () = Lwt_unix.sleep 5. >>= fun _ -> wait ()

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
    let%lwt _ =
      ClientBuilder.(
        init ~token ~intents:Intents.(message_content lor guild_messages) |> build)
      >>= Client.start ~shards:`Autosharded ~with_presence:presence
    in
    wait ()
  in
  Lwt_main.run main
;;

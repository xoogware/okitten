open Okitten
open Lwt.Infix

let () =
  let token =
    match Sys.getenv_opt "TOKEN" with
    | Some t -> t
    | None -> failwith "TOKEN not found in environment"
  in
  Lwt_main.run
    (Logs.set_level @@ Some Logs.Debug;
     Fmt_tty.setup_std_outputs ();
     Logs.set_reporter @@ Logs_fmt.reporter ();
     let%lwt bot =
       Client.ClientBuilder.(
         init ~token ~intents:Intents.(message_content lor guild_messages) |> build)
     in
     Fmt_tty.setup_std_outputs ?style_renderer:(Some `Ansi_tty) ();
     let rec loop () = Lwt_unix.sleep 5. >>= fun _ -> loop () in
     Client.start
       ~shards:`Autosharded
       ~with_presence:
         Presence.
           { since = Some (int_of_float (Unix.time () *. 1000.))
           ; activities =
               [ { name = "OCaml evangelists"
                 ; kind = Listening
                 ; url = None
                 ; created_at = int_of_float @@ (Unix.time () *. 1000.)
                 ; timestamps = None
                 ; application_id = None
                 ; details = None
                 ; state = None
                 }
               ]
           ; status = Online
           ; afk = false
           }
       bot
     >>= fun _ ->
     Logs.info (fun f -> f "Bot started ^_^");
     loop ())
;;

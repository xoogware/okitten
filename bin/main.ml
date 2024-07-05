open Okitten
open Lwt.Infix

let lwt_reporter () =
  let buf_fmt ~like =
    let b = Buffer.create 512 in
    ( Fmt.with_buffer ~like b
    , fun () ->
        let m = Buffer.contents b in
        Buffer.reset b;
        m )
  in
  let app, app_flush = buf_fmt ~like:Fmt.stdout in
  let dst, dst_flush = buf_fmt ~like:Fmt.stderr in
  let reporter = Logs_fmt.reporter ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        match level with
        | Logs.App -> Lwt_io.write Lwt_io.stdout (app_flush ())
        | _ -> Lwt_io.write Lwt_io.stderr (dst_flush ())
      in
      let unblock () =
        over ();
        Lwt.return_unit
      in
      Lwt.finalize write unblock |> Lwt.ignore_result;
      k ()
    in
    reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  { Logs.report }
;;

let () =
  let token =
    match Sys.getenv_opt "TOKEN" with
    | Some t -> t
    | None -> failwith "TOKEN not found in environment"
  in
  Lwt_main.run
    (Logs.set_level @@ Some Logs.Debug;
     Logs.set_reporter @@ lwt_reporter ();
     let%lwt bot = Client.ClientBuilder.(init ~token |> build) in
     Fmt_tty.setup_std_outputs ?style_renderer:(Some `Ansi_tty) ();
     let rec loop () = Lwt_unix.sleep 5. >>= fun _ -> loop () in
     Client.start ~shards:`Autosharded bot
     >>= fun _ ->
     Logs.info (fun f -> f "Bot started ^_^");
     loop ())
;;

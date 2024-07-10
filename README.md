# OKitten

a Discord bot library for OCaml inspired by [serenity](https://github.com/serenity-rs/serenity).

> [!WARNING]
> While OKitten is pre-1.0, the API is VERY unstable. It WILL break when you update it. Good luck!

> [!CAUTION]
> This library is **NOT COMPLETE**! Even if it appears to work, please expect issues such as missed ratelimits and random disconnects. 
> XOOGWARE is not responsible for any issues such as token invalidations that come from using OKitten in its prerelease state.

## Getting Started
Currently, the easiest way to get started with OKitten is by using the [flake](https://nixos.wiki/wiki/Flakes):
```nix
# flake.nix
{
  inputs = {
    # ...
    okitten = {
      url = "github:xoogware/okitten";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  # use inputs.okitten.packages.default here
}
```

Remember to include OKitten using whatever build system you prefer:

```lisp
; dune-project
(package
  (name bot)
  (depends ocaml dune okitten))
```

## Example
Here's a basic bot that connects to Discord and logs messages:
```ocaml
open Okitten

let rec wait () = 
  let%lwt _ = Lwt_unix.sleep 5. in 
  wait ()
;;

let log_ready () = return @@ Logs.info (fun m -> m "Ready!")

let log_message _ctx (msg : Okitten.Models.Message.t) =
  return @@ Logs.info (fun m -> m "Got message: %s" msg.content)
;;


let main =
  let activity =
    Presence.Activity.(
      empty
      |> set_kind Playing
      |> set_name "Ping Pong")
  in
  let presence =
    Presence.(
      empty |> since_now |> with_activity activity |> set_status Idle |> set_afk false)
  in
  let event_handler = 
    EventHandler.(init () |> set_on_ready log_ready |> set_on_message log_message) 
  in
  let%lwt client = ClientBuilder.(
        init
        |> set_token token
        |> set_intents Intents.(message_content lor guild_messages)
        |> set_event_handler event_handler
        |> build)
  in
  let%lwt _ = Client.start ~shards:`Autosharded ~with_presence:presence in
  wait ()
;;

let () = Lwt_main.run main
```

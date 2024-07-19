open Lwt
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t =
  { id : string
  ; channel_id : string
  ; author : User.t
  ; content : string
  ; timestamp : Models.date
  ; edited_timestamp : Models.date option
  ; tts : bool
  ; mention_everyone : bool
  ; mentions : User.t list
  ; guild_id : string option
  }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type message_reference = { message_id : string }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type message_create_payload =
  { content : string option
  ; tts : bool
  ; message_reference : message_reference option [@yojson.option]
  }
[@@deriving yojson]

let create ?original ?content ~channel_id (ctx : Ctx.event_ctx) =
  let message_reference =
    match original with
    | Some original -> Some { message_id = original.id }
    | None -> None
  in
  let req_body =
    { content; tts = false; message_reference }
    |> yojson_of_message_create_payload
    |> Yojson.Safe.to_string
  in
  let route = Printf.sprintf "/channels/%s/messages" channel_id in
  let%lwt res =
    Request.(make ~route ~meth:Post |> body @@ Some req_body) |> Http.fire ~http:ctx.http
  in
  return res
;;

let reply ~original ~content ctx =
  create ~original ~channel_id:original.channel_id ~content ctx
;;

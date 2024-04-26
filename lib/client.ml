open! Lwt

(** The configuration for an OKitten Client. *)
module type ClientConfig = sig
  (** The number of shards to create. [None] for auto sharding. *)
  val shards : int option

  val token : string
end

type t = { sharder : Sharder.t }

module type S = sig
  (** Starts the Client and connects to Discord. *)
  val start : unit -> t Lwt.t
end

module Make (C : ClientConfig) : S = struct
  let shards = C.shards

  let start () =
    Client_options.token := C.token;
    ignore @@ Logs_lwt.info (fun f -> f "OKitten started :3");
    Sharder.start ?count:shards () >|= fun sharder -> { sharder }
  ;;
end

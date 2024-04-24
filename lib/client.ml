open! Lwt

module type ClientConfig = sig
  val shards : int
  val token : string
end

module Make (C : ClientConfig) = struct
  let _shards = C.shards

  let start () =
    Client_options.token := C.token;
    Logs_lwt.info (fun f -> f "OKitten started :3")
  ;;
end

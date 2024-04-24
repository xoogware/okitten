module type ClientConfig = sig
  val shards : int
  val token : string
end

module Make (_ : ClientConfig) : sig
  val start : unit -> unit Lwt.t
end

open Websocket

module Shard : sig
  type state =
    { id : int * int
    ; is_ready : bool ref
    ; seq : int
    ; frame_stream : Frame.t Lwt_stream.t * (Frame.t option -> unit)
    ; conn : Websocket_lwt_unix.conn
    ; heartbeat_interval : int
    }

  type t =
    { state : state
    ; stopped : bool
    ; can_resume : bool
    }

  val heartbeat : state -> state Lwt.t
end

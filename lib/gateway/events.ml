(** Represents a {{:https://discord.com/developers/docs/topics/gateway#gateway-events-example-gateway-event} Frame} sent over the Receive. *)
type 'a frame =
  { op : int
  ; d : 'a
  ; s : int option
  ; t : string option
  }

module Opcode = struct
  type t =
    | Dispatch
    | Heartbeat
    | Identify
    | PresenceUpdate
    | VoiceStateUpdate
    | Resume
    | Reconnect
    | RequestGuildMembers
    | InvalidSession
    | Hello
    | HeartbeatAck

  let to_int = function
    | Dispatch -> 0
    | Heartbeat -> 1
    | Identify -> 2
    | PresenceUpdate -> 3
    | VoiceStateUpdate -> 4
    | Resume -> 6
    | Reconnect -> 7
    | RequestGuildMembers -> 8
    | InvalidSession -> 9
    | Hello -> 10
    | HeartbeatAck -> 11
  ;;

  let of_int = function
    | 0 -> Dispatch
    | 1 -> Heartbeat
    | 2 -> Identify
    | 3 -> PresenceUpdate
    | 4 -> VoiceStateUpdate
    | 6 -> Resume
    | 7 -> Reconnect
    | 8 -> RequestGuildMembers
    | 9 -> InvalidSession
    | 10 -> Hello
    | 11 -> HeartbeatAck
    | _ -> raise (Invalid_argument "Invalid Opcode received")
  ;;

  let to_string = function
    | Dispatch -> "DISPATCH"
    | Heartbeat -> "HEARTBEAT"
    | Identify -> "IDENTIFY"
    | PresenceUpdate -> "PRESENCE_UPDATE"
    | VoiceStateUpdate -> "VOICE_STATE_UPDATE"
    | Resume -> "RESUME"
    | Reconnect -> "RECONNECT"
    | RequestGuildMembers -> "REQUEST_GUILD_MEMBERS"
    | InvalidSession -> "INVALID_SESSION"
    | Hello -> "HELLO"
    | HeartbeatAck -> "HEARTBEAT_ACK"
  ;;
end

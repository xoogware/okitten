type session_start_limit =
  { total : int
  ; remaining : int
  ; reset_after : int
  ; max_concurrency : int
  }
[@@deriving yojson]

type bot_gateway =
  { url : string
  ; shards : int
  ; session_start_limit : session_start_limit
  }
[@@deriving yojson]

type identify_connection =
  { os : string
  ; browser : string
  ; device : string
  }
[@@deriving yojson]

type identify =
  { token : string
  ; properties : identify_connection
  ; compress : bool
  ; large_threshold : int
  ; shard : int * int
  ; intents : int
  ; presence : Presence.t option
  }
[@@deriving yojson]

type hello = { heartbeat_interval : int }
[@@deriving yojson] [@@yojson.allow_extra_fields]

type heartbeat = int option [@@deriving yojson]

module Dispatch = Dispatch

module Opcode = struct
  type t =
    | Dispatch
    | Heartbeat
    | Identify
    | PresenceUpdate
    | Resume
    | Reconnect
    | RequestGuildMembers
    | InvalidSession
    | Hello
    | HeartbeatAck

  let yojson_of_t = function
    | Dispatch -> `Int 0
    | Heartbeat -> `Int 1
    | Identify -> `Int 2
    | PresenceUpdate -> `Int 3
    | Resume -> `Int 6
    | Reconnect -> `Int 7
    | RequestGuildMembers -> `Int 8
    | InvalidSession -> `Int 9
    | Hello -> `Int 10
    | HeartbeatAck -> `Int 11
  ;;

  let of_int = function
    | 0 -> Dispatch
    | 1 -> Heartbeat
    | 2 -> Identify
    | 3 -> PresenceUpdate
    | 6 -> Resume
    | 7 -> Reconnect
    | 8 -> RequestGuildMembers
    | 9 -> InvalidSession
    | 10 -> Hello
    | 11 -> HeartbeatAck
    | _ -> failwith "Invalid opcode"
  ;;

  let t_of_yojson = function
    | `Int c -> of_int c
    | _ -> failwith "Invalid opcode"
  ;;

  let to_string = function
    | Dispatch -> "Dispatch"
    | Heartbeat -> "Heartbeat"
    | Identify -> "Identify"
    | PresenceUpdate -> "PresenceUpdate"
    | Resume -> "Resume"
    | Reconnect -> "Reconnect"
    | RequestGuildMembers -> "RequestGuildMembers"
    | InvalidSession -> "InvalidSession"
    | Hello -> "Hello"
    | HeartbeatAck -> "HeartbeatAck"
  ;;
end

type 'a payload =
  { op : Opcode.t
  ; d : 'a
  ; t : int option
  ; s : int option
  }
[@@deriving yojson]

let payload_of ~op ?s d = { op; d; t = None; s }

(** Flags for intents.
    See {{:https://discord.com/developers/docs/topics/gateway#gateway-intents} Discord's docs} for more info. *)
module Intents = struct
  (** includes:
      - GUILD_CREATE
      - GUILD_UPDATE
      - GUILD_DELETE
      - GUILD_ROLE_CREATE
      - GUILD_ROLE_UPDATE
      - GUILD_ROLE_DELETE
      - CHANNEL_CREATE
      - CHANNEL_UPDATE
      - CHANNEL_DELETE
      - CHANNEL_PINS_UPDATE
      - THREAD_CREATE
      - THREAD_UPDATE
      - THREAD_DELETE
      - THREAD_LIST_SYNC
      - THREAD_MEMBER_UPDATE
      - THREAD_MEMBERS_UPDATE
      - STAGE_INSTANCE_CREATE
      - STAGE_INSTANCE_UPDATE
      - STAGE_INSTANCE_DELETE *)
  let guilds = 1 lsl 0

  (** includes:
      - GUILD_MEMBER_ADD
      - GUILD_MEMBER_UPDATE
      - GUILD_MEMBER_REMOVE
      - THREAD_MEMBERS_UPDATE *)
  let guild_members = 1 lsl 1

  (** includes:
      - GUILD_AUDIT_LOG_ENTRY_CREATE
      - GUILD_BAN_ADD
      - GUILD_BAN_REMOVE *)
  let guild_moderation = 1 lsl 2

  (** includes:
      - GUILD_EMOJIS_UPDATE
      - GUILD_STICKERS_UPDATE *)
  let guild_emojis_and_stickers = 1 lsl 3

  (** includes:
      - GUILD_INTEGRATIONS_UPDATE
      - INTEGRATION_CREATE
      - INTEGRATION_UPDATE
      - INTEGRATION_DELETE *)
  let guild_integrations = 1 lsl 4

  (** includes:
      - WEBHOOKS_UPDATE *)
  let guild_webhooks = 1 lsl 5

  (** includes:
      - INVITE_CREATE
      - INVITE_DELETE *)
  let guild_invites = 1 lsl 6

  (** includes:
      - VOICE_STATE_UPDATE *)
  let guild_voice_states = 1 lsl 7

  (** includes:
      - PRESENCE_UPDATE *)
  let guild_presences = 1 lsl 8

  (** includes:
      - MESSAGE_CREATE
      - MESSAGE_UPDATE
      - MESSAGE_DELETE
      - MESSAGE_DELETE_BULK *)
  let guild_messages = 1 lsl 9

  (** includes:
      - MESSAGE_REACTION_ADD
      - MESSAGE_REACTION_REMOVE
      - MESSAGE_REACTION_REMOVE_ALL
      - MESSAGE_REACTION_REMOVE_EMOJI *)
  let guild_message_reactions = 1 lsl 10

  (** includes:
      - TYPING_START *)
  let guild_message_typing = 1 lsl 11

  (** includes:
      - MESSAGE_CREATE
      - MESSAGE_UPDATE
      - MESSAGE_DELETE
      - CHANNEL_PINS_UPDATE *)
  let direct_messages = 1 lsl 12

  (** includes:
      - MESSAGE_REACTION_ADD
      - MESSAGE_REACTION_REMOVE
      - MESSAGE_REACTION_REMOVE_ALL
      - MESSAGE_REACTION_REMOVE_EMOJI *)
  let direct_message_reactions = 1 lsl 13

  (** includes:
      - TYPING_START *)
  let direct_message_typing = 1 lsl 14

  (** Adds fields to events containing message data. *)
  let message_content = 1 lsl 15

  (** includes:
      - GUILD_SCHEDULED_EVENT_CREATE
      - GUILD_SCHEDULED_EVENT_UPDATE
      - GUILD_SCHEDULED_EVENT_DELETE
      - GUILD_SCHEDULED_EVENT_USER_ADD
      - GUILD_SCHEDULED_EVENT_USER_REMOVE *)
  let guild_scheduled_events = 1 lsl 16

  (** includes:
      - AUTO_MODERATION_RULE_CREATE
      - AUTO_MODERATION_RULE_UPDATE
      - AUTO_MODERATION_RULE_DELETE *)
  let auto_moderation_configuration = 1 lsl 20

  (** includes:
      - AUTO_MODERATION_ACTION_EXECUTION *)
  let auto_moderation_execution = 1 lsl 21

  (** includes:
      - MESSAGE_POLL_VOTE_ADD
      - MESSAGE_POLL_VOTE_REMOVE *)
  let guild_message_polls = 1 lsl 24

  (** includes:
      - MESSAGE_POLL_VOTE_ADD
      - MESSAGE_POLL_VOTE_REMOVE *)
  let direct_message_polls = 1 lsl 25
end

type event =
  | Hello
  | Ready
  | Resumed
  | Reconnect
  | InvalidSession
  | ApplicationCommandPermissionsUpdate
  | AutoModerationRuleCreate
  | AutoModerationRuleUpdate
  | AutoModerationRuleDelete
  | AutoModerationActionExecution
  | ChannelCreate
  | ChannelUpdate
  | ChannelDelete
  | ChannelPinsUpdate
  | ThreadCreate
  | ThreadUpdate
  | ThreadDelete
  | ThreadListSync
  | ThreadMemberUpdate
  | ThreadMembersUpdate
  | EntitlementCreate
  | EntitlementUpdate
  | EntitlementDelete
  | GuildCreate
  | GuildUpdate
  | GuildDelete
  | GuildAuditLogEntryCreate
  | GuildBanAdd
  | GuildBanRemove
  | GuildEmojisUpdate
  | GuildStickersUpdate
  | GuildIntegrationsUpdate
  | GuildMemberAdd
  | GuildMemberRemove
  | GuildMemberUpdate
  | GuildMembersChunk
  | GuildRoleCreate
  | GuildRoleUpdate
  | GuildRoleDelete
  | GuildScheduledEventCreate
  | GuildScheduledEventUpdate
  | GuildScheduledEventDelete
  | GuildScheduledEventUserAdd
  | GuildScheduledEventUserRemove
  | IntegrationCreate
  | IntegrationUpdate
  | IntegrationDelete
  | InteractionCreate
  | InviteCreate
  | InviteDelete
  | MessageCreate
  | MessageUpdate
  | MessageDelete
  | MessageDeleteBulk
  | MessageReactionAdd
  | MessageReactionRemove
  | MessageReactionRemoveAll
  | MessageReactionRemoveEmoji
  | PresenceUpdate
  | StageInstanceCreate
  | StageInstanceUpdate
  | StageInstanceDelete
  | TypingStart
  | UserUpdate
  | VoiceStateUpdate
  | VoiceServerUpdate
  | WebhooksUpdate
  | MessagePollVoteAdd
  | MessagePollVoteRemove

(** Converts an event to its name - from CamelCase to UPPER_SNAKE_CASE *)
let event_to_string = function
  | Hello -> "HELLO"
  | Ready -> "READY"
  | Resumed -> "RESUMED"
  | Reconnect -> "RECONNECT"
  | InvalidSession -> "INVALID_SESSION"
  | ApplicationCommandPermissionsUpdate -> "APPLICATION_COMMAND_PERMISSIONS_UPDATE"
  | AutoModerationRuleCreate -> "AUTO_MODERATION_RULE_CREATE"
  | AutoModerationRuleUpdate -> "AUTO_MODERATION_RULE_UPDATE"
  | AutoModerationRuleDelete -> "AUTO_MODERATION_RULE_DELETE"
  | AutoModerationActionExecution -> "AUTO_MODERATION_ACTION_EXECUTION"
  | ChannelCreate -> "CHANNEL_CREATE"
  | ChannelUpdate -> "CHANNEL_UPDATE"
  | ChannelDelete -> "CHANNEL_DELETE"
  | ChannelPinsUpdate -> "CHANNEL_PINS_UPDATE"
  | ThreadCreate -> "THREAD_CREATE"
  | ThreadUpdate -> "THREAD_UPDATE"
  | ThreadDelete -> "THREAD_DELETE"
  | ThreadListSync -> "THREAD_LIST_SYNC"
  | ThreadMemberUpdate -> "THREAD_MEMBER_UPDATE"
  | ThreadMembersUpdate -> "THREAD_MEMBERS_UPDATE"
  | EntitlementCreate -> "ENTITLEMENT_CREATE"
  | EntitlementUpdate -> "ENTITLEMENT_UPDATE"
  | EntitlementDelete -> "ENTITLEMENT_DELETE"
  | GuildCreate -> "GUILD_CREATE"
  | GuildUpdate -> "GUILD_UPDATE"
  | GuildDelete -> "GUILD_DELETE"
  | GuildAuditLogEntryCreate -> "GUILD_AUDIT_LOG_ENTRY_CREATE"
  | GuildBanAdd -> "GUILD_BAN_ADD"
  | GuildBanRemove -> "GUILD_BAN_REMOVE"
  | GuildEmojisUpdate -> "GUILD_EMOJIS_UPDATE"
  | GuildStickersUpdate -> "GUILD_STICKERS_UPDATE"
  | GuildIntegrationsUpdate -> "GUILD_INTEGRATIONS_UPDATE"
  | GuildMemberAdd -> "GUILD_MEMBER_ADD"
  | GuildMemberRemove -> "GUILD_MEMBER_REMOVE"
  | GuildMemberUpdate -> "GUILD_MEMBER_UPDATE"
  | GuildMembersChunk -> "GUILD_MEMBERS_CHUNK"
  | GuildRoleCreate -> "GUILD_ROLE_CREATE"
  | GuildRoleUpdate -> "GUILD_ROLE_UPDATE"
  | GuildRoleDelete -> "GUILD_ROLE_DELETE"
  | GuildScheduledEventCreate -> "GUILD_SCHEDULED_EVENT_CREATE"
  | GuildScheduledEventUpdate -> "GUILD_SCHEDULED_EVENT_UPDATE"
  | GuildScheduledEventDelete -> "GUILD_SCHEDULED_EVENT_DELETE"
  | GuildScheduledEventUserAdd -> "GUILD_SCHEDULED_EVENT_USER_ADD"
  | GuildScheduledEventUserRemove -> "GUILD_SCHEDULED_EVENT_USER_REMOVE"
  | IntegrationCreate -> "INTEGRATION_CREATE"
  | IntegrationUpdate -> "INTEGRATION_UPDATE"
  | IntegrationDelete -> "INTEGRATION_DELETE"
  | InteractionCreate -> "INTERACTION_CREATE"
  | InviteCreate -> "INVITE_CREATE"
  | InviteDelete -> "INVITE_DELETE"
  | MessageCreate -> "MESSAGE_CREATE"
  | MessageUpdate -> "MESSAGE_UPDATE"
  | MessageDelete -> "MESSAGE_DELETE"
  | MessageDeleteBulk -> "MESSAGE_DELETE_BULK"
  | MessageReactionAdd -> "MESSAGE_REACTION_ADD"
  | MessageReactionRemove -> "MESSAGE_REACTION_REMOVE"
  | MessageReactionRemoveAll -> "MESSAGE_REACTION_REMOVE_ALL"
  | MessageReactionRemoveEmoji -> "MESSAGE_REACTION_REMOVE_EMOJI"
  | PresenceUpdate -> "PRESENCE_UPDATE"
  | StageInstanceCreate -> "STAGE_INSTANCE_CREATE"
  | StageInstanceUpdate -> "STAGE_INSTANCE_UPDATE"
  | StageInstanceDelete -> "STAGE_INSTANCE_DELETE"
  | TypingStart -> "TYPING_START"
  | UserUpdate -> "USER_UPDATE"
  | VoiceStateUpdate -> "VOICE_STATE_UPDATE"
  | VoiceServerUpdate -> "VOICE_SERVER_UPDATE"
  | WebhooksUpdate -> "WEBHOOKS_UPDATE"
  | MessagePollVoteAdd -> "MESSAGE_POLL_VOTE_ADD"
  | MessagePollVoteRemove -> "MESSAGE_POLL_VOTE_REMOVE"
;;

let event_of_string = function
  | "HELLO" -> Hello
  | "READY" -> Ready
  | "RESUMED" -> Resumed
  | "RECONNECT" -> Reconnect
  | "INVALID_SESSION" -> InvalidSession
  | "APPLICATION_COMMAND_PERMISSIONS_UPDATE" -> ApplicationCommandPermissionsUpdate
  | "AUTO_MODERATION_RULE_CREATE" -> AutoModerationRuleCreate
  | "AUTO_MODERATION_RULE_UPDATE" -> AutoModerationRuleUpdate
  | "AUTO_MODERATION_RULE_DELETE" -> AutoModerationRuleDelete
  | "AUTO_MODERATION_ACTION_EXECUTION" -> AutoModerationActionExecution
  | "CHANNEL_CREATE" -> ChannelCreate
  | "CHANNEL_UPDATE" -> ChannelUpdate
  | "CHANNEL_DELETE" -> ChannelDelete
  | "CHANNEL_PINS_UPDATE" -> ChannelPinsUpdate
  | "THREAD_CREATE" -> ThreadCreate
  | "THREAD_UPDATE" -> ThreadUpdate
  | "THREAD_DELETE" -> ThreadDelete
  | "THREAD_LIST_SYNC" -> ThreadListSync
  | "THREAD_MEMBER_UPDATE" -> ThreadMemberUpdate
  | "THREAD_MEMBERS_UPDATE" -> ThreadMembersUpdate
  | "ENTITLEMENT_CREATE" -> EntitlementCreate
  | "ENTITLEMENT_UPDATE" -> EntitlementUpdate
  | "ENTITLEMENT_DELETE" -> EntitlementDelete
  | "GUILD_CREATE" -> GuildCreate
  | "GUILD_UPDATE" -> GuildUpdate
  | "GUILD_DELETE" -> GuildDelete
  | "GUILD_AUDIT_LOG_ENTRY_CREATE" -> GuildAuditLogEntryCreate
  | "GUILD_BAN_ADD" -> GuildBanAdd
  | "GUILD_BAN_REMOVE" -> GuildBanRemove
  | "GUILD_EMOJIS_UPDATE" -> GuildEmojisUpdate
  | "GUILD_STICKERS_UPDATE" -> GuildStickersUpdate
  | "GUILD_INTEGRATIONS_UPDATE" -> GuildIntegrationsUpdate
  | "GUILD_MEMBER_ADD" -> GuildMemberAdd
  | "GUILD_MEMBER_REMOVE" -> GuildMemberRemove
  | "GUILD_MEMBER_UPDATE" -> GuildMemberUpdate
  | "GUILD_MEMBERS_CHUNK" -> GuildMembersChunk
  | "GUILD_ROLE_CREATE" -> GuildRoleCreate
  | "GUILD_ROLE_UPDATE" -> GuildRoleUpdate
  | "GUILD_ROLE_DELETE" -> GuildRoleDelete
  | "GUILD_SCHEDULED_EVENT_CREATE" -> GuildScheduledEventCreate
  | "GUILD_SCHEDULED_EVENT_UPDATE" -> GuildScheduledEventUpdate
  | "GUILD_SCHEDULED_EVENT_DELETE" -> GuildScheduledEventDelete
  | "GUILD_SCHEDULED_EVENT_USER_ADD" -> GuildScheduledEventUserAdd
  | "GUILD_SCHEDULED_EVENT_USER_REMOVE" -> GuildScheduledEventUserRemove
  | "INTEGRATION_CREATE" -> IntegrationCreate
  | "INTEGRATION_UPDATE" -> IntegrationUpdate
  | "INTEGRATION_DELETE" -> IntegrationDelete
  | "INTERACTION_CREATE" -> InteractionCreate
  | "INVITE_CREATE" -> InviteCreate
  | "INVITE_DELETE" -> InviteDelete
  | "MESSAGE_CREATE" -> MessageCreate
  | "MESSAGE_UPDATE" -> MessageUpdate
  | "MESSAGE_DELETE" -> MessageDelete
  | "MESSAGE_DELETE_BULK" -> MessageDeleteBulk
  | "MESSAGE_REACTION_ADD" -> MessageReactionAdd
  | "MESSAGE_REACTION_REMOVE" -> MessageReactionRemove
  | "MESSAGE_REACTION_REMOVE_ALL" -> MessageReactionRemoveAll
  | "MESSAGE_REACTION_REMOVE_EMOJI" -> MessageReactionRemoveEmoji
  | "PRESENCE_UPDATE" -> PresenceUpdate
  | "STAGE_INSTANCE_CREATE" -> StageInstanceCreate
  | "STAGE_INSTANCE_UPDATE" -> StageInstanceUpdate
  | "STAGE_INSTANCE_DELETE" -> StageInstanceDelete
  | "TYPING_START" -> TypingStart
  | "USER_UPDATE" -> UserUpdate
  | "VOICE_STATE_UPDATE" -> VoiceStateUpdate
  | "VOICE_SERVER_UPDATE" -> VoiceServerUpdate
  | "WEBHOOKS_UPDATE" -> WebhooksUpdate
  | "MESSAGE_POLL_VOTE_ADD" -> MessagePollVoteAdd
  | "MESSAGE_POLL_VOTE_REMOVE" -> MessagePollVoteRemove
  | _ -> failwith "Unknown event"
;;

let event_of_yojson = function
  | `String s -> event_of_string s
  | _ -> failwith "Not a string"
;;

let event_to_yojson event = `String (event_to_string event)

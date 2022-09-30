{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

{-
    Handle Permissions with the discord api hex values to check if needed permissions are set
-}

module Permissions where
import Discord.Internal.Rest ( Role(rolePerms, roleId), GuildMember (memberRoles), Guild (guildRoles), RoleId )
import Data.Text as T ( unpack )
import Data.Bits ( Bits((.&.), shift) )

roleIdToRole :: Guild -> RoleId -> [Role]
roleIdToRole  g r = filter(\x -> roleId x == r) $ guildRoles g
-- | Return list of Roles, which should only be the length of 1


data Permissions =
    CREATE_INSTANT_INVITE | KICK_MEMBERS | BAN_MEMBERS | ADMINISTRATOR | MANAGE_CHANNELS |MANAGE_GUILD |ADD_REACTIONS |VIEW_AUDIT_LOG |PRIORITY_SPEAKER |STREAM |VIEW_CHANNEL |SEND_MESSAGES  |SEND_TTS_MESSAGES |MANAGE_MESSAGES|EMBED_LINKS |ATTACH_FILES  |READ_MESSAGE_HISTORY|MENTION_EVERYONE |USE_EXTERNAL_EMOJIS |VIEW_GUILD_INSIGHT|CONNECT |SPEAK |MUTE_MEMBERS |DEAFEN_MEMBERS |MOVE_MEMBERS |USE_VAD |CHANGE_NICKNAME  |MANAGE_NICKNAMES |MANAGE_ROLES  |MANAGE_WEBHOOKS  |MANAGE_EMOJIS_AND_STICKERS  |USE_APPLICATION_COMMANDS    |REQUEST_TO_SPEAK      |MANAGE_EVENTS         |MANAGE_THREADS        |CREATE_PUBLIC_THREADS |CREATE_PRIVATE_THREADS|USE_EXTERNAL_STICKERS |SEND_MESSAGES_IN_THREADS    |USE_EMBEDDED_ACTIVITIES
  deriving (Enum,Show)
-- | Formatting seems to be like that. fromEnum would otherwise return only 1


hasRolePermission :: Role -> Permissions -> Bool
hasRolePermission r p = (.&.) (read (T.unpack $ rolePerms r) :: Int) (shift 1 $ fromEnum p) > 0
-- | Check if a given role has the permission
--   RolePerms need to be an int to be converted into its bits
--   Bitwise & checks if rolePermission contains the perm

hasGuildMemberPermission :: Guild -> GuildMember -> Permissions -> Bool
hasGuildMemberPermission g gm p = or $ (`hasRolePermission` p) <$> concatMap (roleIdToRole g) (memberRoles gm)
-- | Iterate over all roles an user has. Every user has at least one role (@everyone)
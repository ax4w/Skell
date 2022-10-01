{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

{-
    Handle Permissions with the discord api hex values to check if needed permissions are set
-}

module Permissions where
import Discord.Internal.Rest ( Role(rolePerms, roleId), GuildMember (memberRoles), Guild (guildRoles), RoleId )
import Data.Text as T ( unpack )
import Data.Bits ( Bits((.&.), shift) )
import Data.Bool
import Data.Maybe
import Data.List

-- | If there is no such role on the guild return nothing
--   otherwise return the role. Take the head of the list. List should always be one, because the ID is unique
roleIdToRole :: Guild -> RoleId -> Maybe Role
roleIdToRole  g r = find(\x -> roleId x == r) $ guildRoles g

-- | Enum for the discord permissions, index of enum is used for the bit shifting
data Permissions =
    CREATE_INSTANT_INVITE
  | KICK_MEMBERS
  | BAN_MEMBERS
  | ADMINISTRATOR
  | MANAGE_CHANNELS
  | MANAGE_GUILD
  | ADD_REACTIONS
  | VIEW_AUDIT_LOG
  | PRIORITY_SPEAKER
  | STREAM
  | VIEW_CHANNEL
  | SEND_MESSAGES
  | SEND_TTS_MESSAGES
  | MANAGE_MESSAGES
  | EMBED_LINKS
  | ATTACH_FILES
  | READ_MESSAGE_HISTORY
  | MENTION_EVERYONE
  | USE_EXTERNAL_EMOJIS
  | VIEW_GUILD_INSIGHT
  | CONNECT
  | SPEAK
  | MUTE_MEMBERS
  | DEAFEN_MEMBERS
  | MOVE_MEMBERS
  | USE_VAD
  | CHANGE_NICKNAME
  | MANAGE_NICKNAMES
  | MANAGE_ROLES
  | MANAGE_WEBHOOKS
  | MANAGE_EMOJIS_AND_STICKERS
  | USE_APPLICATION_COMMANDS
  | REQUEST_TO_SPEAK
  | MANAGE_EVENTS
  | MANAGE_THREADS
  | CREATE_PUBLIC_THREADS
  | CREATE_PRIVATE_THREADS
  | USE_EXTERNAL_STICKERS
  | SEND_MESSAGES_IN_THREADS
  | USE_EMBEDDED_ACTIVITIES
  deriving (Enum,Show)


-- | Check if a given role has the permission
--   RolePerms need to be an int to be converted into its bits
--   Bitwise & checks if rolePermission contains the perm
hasRolePermission :: Role -> Permissions -> Bool
hasRolePermission r p = (.&.) (read (T.unpack $ rolePerms r) :: Int) (shift 1 $ fromEnum p) > 0


-- | Check if any Role of an GuildMember has the needed permission
--   If the result of roleIdToRole is Nothing, it appends an "False"
--   Otherwise it checks for the needed permission
hasGuildMemberPermission :: Guild -> GuildMember -> Permissions -> Bool
hasGuildMemberPermission g gm p = or $ go (memberRoles gm) g
  where
    go [] _ = []
    go (x:xs) g = case roleIdToRole g x of
                    Nothing ->  [False] <> go xs g
                    Just a ->   [a `hasRolePermission` p] <> go xs g
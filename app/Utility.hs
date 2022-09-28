{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Utility where

import Discord.Types
    ( Message(messageContent, messageGuildId, messageAuthor, messageMentions, messageMentionRoles, messageMember, messageChannelId), Role (roleId, roleName, rolePerms), Guild (guildRoles), User (userDiscrim, userMember), GuildMember (GuildMember, memberPermissions, memberJoinedAt, memberRoles), GuildId, GuildBan (guildBanReason), UserId, DiscordColor (DiscordColorPurple), CreateEmbed (createEmbedTitle), RoleId)
import qualified Data.Text as T
import Data.Char ( isDigit )
import Data.Maybe (fromMaybe)
import Data.String ( IsString(fromString) )
import Data.Text (Text, isPrefixOf)
import qualified Discord.Internal.Rest.Guild as G
import qualified Discord.Internal.Rest.Channel as R
import Discord ( def )
import Data.Bits ((.&.), Bits)
import Data.Bool (bool)

prefix :: Text
prefix = ">"

embedColor :: DiscordColor
embedColor = DiscordColorPurple

getArgCount :: Message -> Int
getArgCount m = length (T.words $ messageContent m)

getArg :: Message -> Int -> T.Text
getArg m i = T.words (messageContent m) !! i

isInt :: T.Text -> Bool
isInt t = all isDigit (T.unpack t)

argToInt :: Message -> Int -> Int
argToInt m i = read $ filter isDigit $ T.unpack $ getArg m i  :: Int

renderBans :: [(Text,Text)] -> [Text]
renderBans [] = []
renderBans ((a,b):xs) = [fromString "User **" <> a <> fromString "** banned for Reason: **" <> b <> fromString "**"] <> renderBans xs

validateRenderBans :: [Text] -> [Text]
validateRenderBans t | not(null t) = t
                    | otherwise = [T.pack "No Bans Recorded"]

buildGuildImgFromHash :: GuildId -> Text -> Text
buildGuildImgFromHash g t
    | t == "" = ""
    | otherwise = fromString "https://cdn.discordapp.com/icons/" <> T.pack (show g) <> T.pack "/" <> t <> fileEnding t

buildUserImgFromHash :: UserId -> Text -> Text
buildUserImgFromHash g t
    | t == T.pack "" = T.pack "https://i.pinimg.com/736x/7c/8f/47/7c8f476123d28d103efe381543274c25.jpg"
    | otherwise = fromString "https://cdn.discordapp.com/avatars/" <> T.pack (show g) <> T.pack "/" <> t <> fileEnding t


fileEnding :: Text -> Text
fileEnding t
    | "a_" `isPrefixOf` t = ".gif"
    | otherwise = ".png"



getUserRolesName :: GuildMember -> [Role] -> [T.Text]
getUserRolesName g r= map (("@" <>). roleName) (filter(\x -> roleId x `elem` memberRoles g) r)

getUserRoles :: GuildMember -> [Role] -> [Role]
getUserRoles g = filter(\x -> roleId x `elem` memberRoles g)

hasMentions :: Message -> Bool
hasMentions m = not (null (messageMentions m))

getFirstMention :: Message -> User
getFirstMention m = head (messageMentions m)

hasRoleMentions :: Message -> Bool
hasRoleMentions m = not (null (messageMentionRoles m))

getFirstRoleMention :: Message -> RoleId
getFirstRoleMention m = head (messageMentionRoles m)

getRoleFromGuild :: Guild -> RoleId -> Role
getRoleFromGuild g r = guildRoles  g !! f
    where
        f = length $ takeWhile(/= r)  $ roleId <$> guildRoles  g

selectGuildMemerFromMsg :: Message -> Maybe GuildMember
selectGuildMemerFromMsg m
    | hasMentions m = userMember  $ getFirstMention m
    | otherwise = messageMember m

selectUserFromMsg :: Message -> User
selectUserFromMsg m
    | hasMentions m =  getFirstMention m
    | otherwise = messageAuthor  m

err :: Message -> R.ChannelRequest Message
err m = R.CreateMessageDetailed (messageChannelId m) def {
          R.messageDetailedEmbeds = Just
          [ def{createEmbedTitle = "Oh! Looks like an error occurred"}]
}

{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text
module Cmds.Kick where


import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageId, messageChannelId, messageGuildId, messageAuthor, messageMember, messageMentions),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedImage, createEmbedFields, createEmbedThumbnail, createEmbedFooterText, createEmbedColor),
      CreateEmbedImage(CreateEmbedImageUrl), Role (roleName, rolePerms), 
      EmbedField (EmbedField), 
      Guild (guildName, guildBanner, guildIcon, guildChannels, guildNSFWLevel, guildDescription, guildAfkTimeout, guildRoles), 
      Channel (channelName), 
      User (userName, userDiscrim, userIsBot, userMember, userId, userAvatar))
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import qualified Discord.Internal.Rest.Guild as G
import qualified Data.String as T
import Utility (embedColor, selectGuildMemerFromMsg, selectUserFromMsg, err, getUserRoles, hasMentions, getFirstMention, prefix )
import Data.Maybe ()
import Control.Monad.Trans.Except ( runExceptT, ExceptT(ExceptT) )
import Permissions (hasGuildMemberPermission, Permissions (KICK_MEMBERS, ADMINISTRATOR) )

exec:: Message -> DiscordHandler ()
exec m = do
    let synerr = R.CreateMessageDetailed (messageChannelId m) def {
            R.messageDetailedEmbeds = Just
                [ def
                    {   createEmbedTitle = "Oops!",
                        createEmbedColor = Just embedColor,
                        createEmbedFields =
                            [
                              EmbedField "Syntax"
                                ("Syntax: **" <> prefix <> "kick <@user>**") (Just False),
                              EmbedField "Needed Permissions"
                                "KICK_MEMBERS or ADMINISTRATOR" (Just False)
                            ]
                    }
                ]
            }
    Just guildid' <- pure $ messageGuildId m
    Just msgMem <- pure $ messageMember  m

    ma <- runExceptT $ do
        guild' <- ExceptT $ restCall (G.GetGuild guildid')
        _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
        if ( hasGuildMemberPermission guild' msgMem KICK_MEMBERS || hasGuildMemberPermission guild' msgMem ADMINISTRATOR)
            && hasMentions m  then 
                do
                    threadDelay (2 * 10 ^ (2 :: Int))
                    _ <- ExceptT $ restCall (G.RemoveGuildMember guildid' (userId $ getFirstMention m))
                    threadDelay (2 * 10 ^ (2 :: Int))            
                    _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "white_check_mark")
                    return ()
        else 
            do
                _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "warning")
                
                _ <- ExceptT $ restCall synerr
                return ()
    case ma of
      Left e -> void $ restCall $ err m
      Right r -> return()




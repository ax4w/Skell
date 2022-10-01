{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Cmds.UserInfo where

import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageId, messageChannelId, messageGuildId, messageAuthor, messageMember, messageMentions),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedImage, createEmbedFields, createEmbedThumbnail, createEmbedFooterText, createEmbedColor),
      CreateEmbedImage(CreateEmbedImageUrl), Role (roleName, rolePerms), EmbedField (EmbedField), Guild (guildName, guildBanner, guildIcon, guildChannels, guildNSFWLevel, guildDescription, guildAfkTimeout, guildRoles), Channel (channelName), User (userName, userDiscrim, userIsBot, userMember, userId, userAvatar), GuildMember (memberJoinedAt, memberPermissions, memberUser, memberNick), GuildBan (guildBanUser, guildBanReason) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import qualified Discord.Internal.Rest.Guild as G
import qualified Data.String as T
import Utility ( renderBans, buildUserImgFromHash, validateRenderBans,embedColor, getUserRolesName, selectGuildMemerFromMsg, selectUserFromMsg, err, getUserRoles )
import Data.Maybe ( fromMaybe )
import Control.Monad.Trans.Except ( runExceptT, ExceptT(ExceptT) )

exec:: Message -> DiscordHandler ()
exec m = do
    

    Just guildid' <- pure $ messageGuildId m
    Just msgMem <- pure $ selectGuildMemerFromMsg m
    --thanks to @qrpnxz#6636 on the FP-Discord for telling me about that runExceptT thing!
    ma <- runExceptT $ do
      guild' <- ExceptT $ restCall (G.GetGuild guildid')
      _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
      threadDelay (2 * 10 ^ (2 :: Int))
      _ <- ExceptT $
              restCall (R.CreateMessageDetailed (messageChannelId m) def {
                    R.messageDetailedEmbeds = Just
                      [ def
                        { createEmbedTitle = userName (selectUserFromMsg m) <> "#" <> fromMaybe " " (userDiscrim $ selectUserFromMsg m),
                          createEmbedDescription  ="Joined at: " <> T.pack (show $ memberJoinedAt msgMem),
                          createEmbedColor = Just embedColor,
                          createEmbedThumbnail = Just
                             (CreateEmbedImageUrl $ buildUserImgFromHash (userId $ selectUserFromMsg m) (fromMaybe (T.pack "") (userAvatar $ selectUserFromMsg m))),
                          createEmbedFields =
                            [
                              EmbedField "Nickname" (
                                fromMaybe "No nickname set" $ memberNick  msgMem
                              ) (Just True),
                              EmbedField "Roles" (
                                T.intercalate "\n" (
                                getUserRolesName msgMem (guildRoles guild'))
                              ) (Just False)
                              
                            ]
                        }
                      ]
              })
      return ()
    case ma of
      Left e -> void $ restCall $ err m
      Right r -> return()


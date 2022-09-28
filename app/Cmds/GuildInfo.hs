{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Cmds.GuildInfo where

import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageId, messageChannelId, messageGuildId, messageAuthor),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedImage, createEmbedFields, createEmbedThumbnail, createEmbedFooterText, createEmbedColor),
      CreateEmbedImage(CreateEmbedImageUrl), Role (roleName), EmbedField (EmbedField), Guild (guildName, guildBanner, guildIcon, guildChannels, guildNSFWLevel, guildDescription, guildAfkTimeout, guildEmojis), Channel (channelName), User (userName, userDiscrim, userIsBot, userMember), GuildMember (memberJoinedAt), GuildBan (guildBanUser, guildBanReason), Emoji (emojiName) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import qualified Discord.Internal.Rest.Guild as G
import qualified Data.String as T
import Utility ( renderBans, buildGuildImgFromHash, validateRenderBans,embedColor, err )
import Data.Maybe
import Control.Monad.Trans.Except
import Data.Bool (bool)

exec:: Message -> DiscordHandler ()
exec m = do
    Just guildid' <- pure $ messageGuildId m

    --thanks to @qrpnxz#6636 on the FP-Discord for telling me about that runExceptT thing!
    ma <- runExceptT $ do
      guild' <- ExceptT $ restCall (G.GetGuild guildid')
      gbans <- ExceptT $ restCall (G.GetGuildBans guildid')
      gchannels <- ExceptT $ restCall(G.GetGuildChannels guildid')
      groles <- ExceptT $ restCall(G.GetGuildRoles guildid')
      _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
      threadDelay (2 * 10 ^ (2 :: Int))
      _ <- ExceptT $
              restCall (R.CreateMessageDetailed (messageChannelId m) def {
                    R.messageDetailedEmbeds = Just
                      [ def
                        { createEmbedTitle = guildName guild',
                          createEmbedColor = Just embedColor,
                          createEmbedDescription = fromMaybe (T.pack "") (guildDescription guild'),
                          createEmbedThumbnail = Just
                            (CreateEmbedImageUrl $ buildGuildImgFromHash guildid' (fromMaybe (T.pack "") (guildIcon guild'))),
                          createEmbedImage = Just
                            (CreateEmbedImageUrl $ buildGuildImgFromHash guildid' (fromMaybe (T.pack "") (guildBanner guild'))),
                          createEmbedFields =
                            [
                              EmbedField "Bans"  (
                                T.intercalate "\n" $
                                validateRenderBans $
                                renderBans $
                                zip (userName . guildBanUser <$> gbans)(guildBanReason <$> gbans)
                              ) (Just False),
                              EmbedField "Channels" (
                                T.intercalate "\n" $
                                filter(\x -> x /= "Text Channels" && x /= "Voice Channels") $ channelName <$> gchannels
                              ) (Just True),
                              EmbedField "Roles" (
                                T.intercalate "\n@" $
                                map roleName groles
                              ) (Just True),
                              EmbedField "AFK-Timeout" (
                                T.pack (show $ guildAfkTimeout guild' `div` 60) <> T.pack " Minutes"
                              ) (Just True),
                              EmbedField "Emojis" (
                                (\x -> bool ( T.intercalate ", " $ (\y -> ":" <> y <> ":") <$> x) "No custom server emojis found" (null x)) $ emojiName <$> guildEmojis guild'
                              ) (Just False)
                            ]
                        }
                      ]
              })

      return ()
    case ma of
      Left e -> void $ restCall $ err m
      Right r -> return()


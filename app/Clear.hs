{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Clear where

import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageChannelId, messageId, messageContent, messageGuildId, messageAuthor, messageMember),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedFooterText, createEmbedColor), Guild (guildRoles) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import Data.Char ()
import Utility ( getArgCount, getArg, isInt, argToInt,embedColor, err, getUserRoles, hasUserPermissions )
import Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT))
import qualified Discord.Internal.Rest.Guild as G


exec:: Message -> DiscordHandler ()
exec m = do
        let synerr = R.CreateMessageDetailed (messageChannelId m) def {
            R.messageDetailedEmbeds = Just
                [ def
                    {   createEmbedTitle = "Wrong usage - Syntax: ** >clear <amount> **",
                        createEmbedColor = Just embedColor,
                        createEmbedDescription = "The maximum amount is 100",
                        createEmbedFooterText = "If the provided span contains messages, which are too old, then the command will not be executed!"
                    }
                ]
            }
        --msgMem <- messageAuthor m
        Just msgMem <- pure $ messageMember m
        Just guildid' <- pure $ messageGuildId m
        --getUserRoles GuildMember ([Role])
        if getArgCount m == 2 && isInt (getArg m 1) && argToInt m 1 <= 100 && argToInt m 1 >= 0 then do
            ma <- runExceptT $ do
                guild' <- ExceptT $ restCall (G.GetGuild guildid')
                if any(> 0) (hasUserPermissions (getUserRoles msgMem (guildRoles guild')) 0x0000000000002000) then do
                    _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
                    threadDelay (2 * 10 ^ (6 :: Int))
                    _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "white_check_mark")
                    msgs <- ExceptT $  restCall (R.GetChannelMessages (messageChannelId m) (argToInt m 1, R.AroundMessage (messageId m)))
                    _ <- ExceptT $ restCall (R.BulkDeleteMessage (messageChannelId m, map messageId msgs))
                    return ()
                else do
                    _ <- ExceptT $ restCall $ err m
                    return ()

            case ma of
                Left e -> void $ restCall $ err m
                Right r -> return()

        else do
            void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "warning")
            void $ restCall synerr




{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Cmds.Clear where

import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageChannelId, messageId, messageContent, messageGuildId, messageAuthor, messageMember),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedFooterText, createEmbedColor, createEmbedFields), Guild (guildRoles), EmbedField (EmbedField) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import Data.Char ()
import Utility ( getArgCount, getArg, isInt, argToInt,embedColor, err, getUserRoles, prefix )
import Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT))
import qualified Discord.Internal.Rest.Guild as G
import Permissions
    (Permissions (KICK_MEMBERS, ADMINISTRATOR, MANAGE_MESSAGES), hasGuildMemberPermission )


exec:: Message -> DiscordHandler ()
exec m = do
        let synerr = R.CreateMessageDetailed (messageChannelId m) def {
            R.messageDetailedEmbeds = Just
                [ def
                    {   createEmbedTitle = "Oops!",--"Wrong usage - Syntax: ** " <> prefix <>"clear <amount(1-00)> **",
                        createEmbedColor = Just embedColor,
                        createEmbedFooterText = "If the provided span contains messages, which are too old, then the command will not be executed!",
                        createEmbedFields =
                            [
                              EmbedField "Syntax"
                                ("Syntax: **" <> prefix <> "clear <amount[1-100]>**") (Just False),
                              EmbedField "Needed Permissions"
                                "CLEAR_MESSAGES or ADMINISTRATOR" (Just False)
                            ]
                    }
                ]
            }
        Just msgMem <- pure $ messageMember m
        Just guildid' <- pure $ messageGuildId m
        ma <- runExceptT $ do
            guild' <- ExceptT $ restCall (G.GetGuild guildid')
            _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
            if getArgCount m == 2 && isInt (getArg m 1) && argToInt m 1 <= 100 && argToInt m 1 >= 0
                && ( hasGuildMemberPermission guild' msgMem MANAGE_MESSAGES || hasGuildMemberPermission guild' msgMem ADMINISTRATOR)  then 
                    do
                        threadDelay (2 * 10 ^ (6 :: Int))
                        _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "white_check_mark")
                        msgs <- ExceptT $  restCall (R.GetChannelMessages (messageChannelId m) (argToInt m 1, R.AroundMessage (messageId m)))
                        _ <- ExceptT $ restCall (R.BulkDeleteMessage (messageChannelId m, map messageId msgs))
                        return ()
            else 
                do
                    _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "warning")
                    _ <- ExceptT $ restCall synerr
                    return ()

        case ma of
            Left e -> void $ restCall $ err m
            Right r -> return()




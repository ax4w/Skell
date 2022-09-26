{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Clear where

import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageChannelId, messageId, messageContent, messageGuildId),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedFooterText, createEmbedColor) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import Data.Char ()
import Utility ( getArgCount, getArg, isInt, argToInt,embedColor, err )


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
        if getArgCount m == 2 && isInt (getArg m 1) && argToInt m 1 <= 100 && argToInt m 1 >= 0 then do
            void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
            threadDelay (2 * 10 ^ (6 :: Int))
            msgsR <- restCall (R.GetChannelMessages (messageChannelId m) (argToInt m 1, R.AroundMessage (messageId m)))
            case msgsR of
                Left e -> void $ restCall $ err m
                Right msgs -> do
                    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "white_check_mark")
                    void $ restCall (R.BulkDeleteMessage (messageChannelId m, map messageId msgs))
        else do
            void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "warning")
            void $ restCall synerr




{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Cmds.Ping where

import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageId, messageChannelId),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedImage, createEmbedColor),
      CreateEmbedImage(CreateEmbedImageUrl) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import Utility ( embedColor )

exec:: Message -> DiscordHandler ()
exec m = do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    threadDelay (2 * 10 ^ (6 :: Int))
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) def {
        R.messageDetailedEmbeds = Just
            [ def
                { createEmbedTitle = "Pong",
                    createEmbedDescription = "ponggers",
                    createEmbedColor = Just embedColor,
                    createEmbedImage = Just
                    (CreateEmbedImageUrl "https://tcf.admeen.org/category/3500/3005/400x400/ping-pong.jpg")
                }
            ]
        })
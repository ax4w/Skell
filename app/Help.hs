{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Help where

import Utility (prefix, embedColor)
import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageId, messageChannelId),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedImage, createEmbedFields, createEmbedColor),
      CreateEmbedImage(CreateEmbedImageUrl), EmbedField (EmbedField) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )

exec:: Message -> DiscordHandler ()
exec m = do
    void $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
    threadDelay (2 * 10 ^ (6 :: Int))
    void $ restCall (R.CreateMessageDetailed (messageChannelId m) def {
        R.messageDetailedEmbeds = Just
            [ def
                { createEmbedTitle = "Help is on the way!",
                  createEmbedColor = Just embedColor,
                  createEmbedFields =
                            [
                              EmbedField (prefix <> T.pack "Ping")
                                "The legendary ping command" (Just False),
                              EmbedField (prefix <> T.pack "gldinfo")
                                "Some infos about the guild (server)" (Just False),
                              EmbedField (prefix <> T.pack "usrinfo")
                                ("Some infos about the usr\nSyntax: **" <> prefix <> "usrinfo <optional @user>**")  (Just False),
                              EmbedField (prefix <> T.pack "clear") 
                                ("Clear messages\nSytnax: **" <> prefix <> "clear <count(1-100)>**") (Just False),
                              EmbedField (prefix <> T.pack "roleinfo") 
                                ("Role info\nSytnax: **" <> prefix <> "roleinfo <@role>**") (Just False)
                            ]
                }
            ]
        })
{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Cmds.RoleInfo where


import Discord ( restCall, def, DiscordHandler )
import Discord.Types
    ( Message(messageChannelId, messageId, messageContent, messageGuildId, messageAuthor, messageMember),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedFooterText, createEmbedColor, createEmbedThumbnail, createEmbedFields), Guild (guildRoles), CreateEmbedImage (CreateEmbedImageUrl), EmbedField (EmbedField), Role (roleName, roleColor, roleHoist, rolePos, rolePerms) )
import qualified Discord.Requests as R
import Control.Monad (when, void)
import qualified Data.Text as T
import UnliftIO.Concurrent ( threadDelay )
import Data.Char ()
import Utility ( getArgCount, getArg, isInt, argToInt,embedColor, err, getUserRoles, hasUserPermissions, prefix, hasRoleMentions, getFirstMention, getFirstRoleMention, getRoleFromGuild )
import Control.Monad.Trans.Except (runExceptT, ExceptT (ExceptT))
import qualified Discord.Internal.Rest.Guild as G


exec:: Message -> DiscordHandler ()
exec m = do
        let synerr = R.CreateMessageDetailed (messageChannelId m) def {
            R.messageDetailedEmbeds = Just
                [ def
                    {   createEmbedTitle = "Wrong usage - Syntax: ** " <> prefix <> "roleinfo <@role> **",
                        createEmbedColor = Just embedColor
                    }
                ]
            }
        -- Just msgMem <- pure $ messageMember m
        Just guildid' <- pure $ messageGuildId m
        ma <- runExceptT $ do
            if hasRoleMentions m then do
                    guild' <- ExceptT $ restCall (G.GetGuild guildid')
                    _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "eyes")
                    threadDelay (2 * 10 ^ (2 :: Int))
                    _ <- ExceptT $
                            restCall (R.CreateMessageDetailed (messageChannelId m) def {
                                    R.messageDetailedEmbeds = Just
                                    [ def
                                        { createEmbedTitle = "RoleInfo for " <> roleName (getRoleFromGuild guild' $ getFirstRoleMention m),
                                        createEmbedColor = Just $ roleColor (getRoleFromGuild guild' $ getFirstRoleMention m),
                                        createEmbedFields =
                                            [
                                            EmbedField "Is role pinned?" (
                                                T.pack $ show $ roleHoist (getRoleFromGuild guild' $ getFirstRoleMention m)
                                            ) (Just True),
                                            EmbedField "Role position" (
                                                T.pack $ show $ rolePos (getRoleFromGuild guild' $ getFirstRoleMention m)
                                            ) (Just True),
                                            EmbedField "Role permissions in Bit" (
                                                rolePerms (getRoleFromGuild guild' $ getFirstRoleMention m)
                                            ) (Just True)
                                            ]
                                        }
                                    ]
                            })
                    return ()
            else do
                _ <- ExceptT $ restCall (R.CreateReaction (messageChannelId m, messageId m) "warning")
                _ <- ExceptT $ restCall synerr
                return ()

        case ma of
            Left e -> void $ restCall $ err m
            Right r -> return()





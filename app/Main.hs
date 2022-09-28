{-# LANGUAGE OverloadedStrings #-}  -- allows "strings" to be Data.Text

module Main where

import Ping ( exec )
import Clear ( exec )
import GuildInfo ( exec )
import UserInfo ( exec )
import RoleInfo ( exec )
import Help ( exec )
import Utility (prefix)
import Control.Monad (when, void)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Text.Read (readMaybe)

import UnliftIO (liftIO)
import UnliftIO.Concurrent ( threadDelay )

import Discord
    ( restCall,
      runDiscord,
      sendCommand,
      def,
      DiscordHandler,
      RunDiscordOpts(discordToken, discordOnStart, discordOnEnd,
                     discordOnEvent, discordOnLog, discordGatewayIntent) )
import Discord.Types
    ( Message(messageChannelId, messageId, messageAuthor,
              messageContent, messageGuildId),
      CreateEmbed(createEmbedTitle, createEmbedDescription,
                  createEmbedFooterText),
      Event(MessageCreate),
      GatewayIntent(gatewayIntentMembers, gatewayIntentPresences),
      GatewaySendable(UpdateStatus),
      UpdateStatusOpts(UpdateStatusOpts, updateStatusOptsSince,
                       updateStatusOptsGame, updateStatusOptsNewStatus,
                       updateStatusOptsAFK),
      UpdateStatusType(UpdateStatusOnline),
      Activity(activityName, activityType),
      ActivityType(ActivityTypeCompeting, ActivityTypeWatching),
      GuildId,
      User(userIsBot, userMember), GuildMember (memberRoles, memberUser), Role (rolePerms, roleName) )
import qualified Discord.Requests as R
import Discord.Internal.Rest.Channel (MessageTiming(AfterMessage))
import qualified Data.Text.Lazy.Builder.Int as T
import Data.Char (isDigit)
import qualified Discord.Internal.Rest.Guild as G

getToken :: IO T.Text
getToken = TIO.readFile "./secrets/bot.secret"

getGuildId :: IO GuildId
getGuildId = do
  gids <- readFile "./secrets/guildid.secret"
  case readMaybe gids of
    Just g -> pure g
    Nothing -> error "could not read guild id from `guildid.secret`"

main :: IO ()
main = skell

skell :: IO ()
skell = do
  tok <- getToken
  server <- getGuildId

  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler server
                          , discordOnEnd = threadDelay (round (0.4 * 10^6)) >>  putStrLn "Ended"
                          , discordOnEvent = eventHandler
                          , discordOnLog = \s -> TIO.putStrLn s >> TIO.putStrLn ""
                          , discordGatewayIntent = def {gatewayIntentMembers = True, gatewayIntentPresences =True}
                          }
  TIO.putStrLn err

startHandler :: GuildId -> DiscordHandler ()
startHandler server = do
  liftIO $ putStrLn "Started Skell"

  let activity = def { activityName = "I am trying to be written in Haskell"
                     , activityType = ActivityTypeWatching
                     }
  let opts = UpdateStatusOpts { updateStatusOptsSince = Nothing
                              , updateStatusOptsGame = Just activity
                              , updateStatusOptsNewStatus = UpdateStatusOnline
                              , updateStatusOptsAFK = False
                              }
  sendCommand (UpdateStatus opts)

eventHandler :: Event -> DiscordHandler ()
eventHandler event = case event of
      MessageCreate m
          | not (fromBot m) && isCommand m "ping" -> Ping.exec m
          | not (fromBot m) && isCommand m "clear" -> Clear.exec m
          | not (fromBot m) && isCommand m "gldinfo" -> GuildInfo.exec m
          | not (fromBot m) && isCommand m "usrinfo" -> UserInfo.exec m
          | not (fromBot m) && isCommand m "roleinfo" -> RoleInfo.exec m
          | not (fromBot m) && isCommand m "help" -> Help.exec m
      _ -> return ()

fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor

isCommand :: Message -> T.Text -> Bool
isCommand m s = ((prefix <> s) `T.isPrefixOf`) . T.toLower $ messageContent m





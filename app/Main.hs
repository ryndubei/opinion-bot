{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text.IO as T
import System.IO (stderr)
import Discord
import Discord.Types
import Control.Monad.IO.Class (liftIO)

import Negotiator (startHandler, eventHandler)
import Paths_opinion_bot (getDataFileName)

main :: IO ()
main = do
  tok <- T.readFile =<< getDataFileName ".secrets/auth-token.secret"
  testServerId <- getDataFileName ".secrets/guildid.secret"
  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler testServerId
                          , discordOnEnd = liftIO $ putStrLn "Ended"
                          , discordOnEvent = eventHandler testServerId
                          , discordOnLog = \s -> T.hPutStrLn stderr s >> T.hPutStrLn stderr ""
                          , discordGatewayIntent = 
                            def { gatewayIntentMessageContent = True
                                , gatewayIntentMessageChanges = True
                                } -- intents: 33280
                          }
  T.hPutStrLn stderr err
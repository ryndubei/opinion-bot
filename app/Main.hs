{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (killThread)
import qualified Data.Text.IO as T
import Discord
import Discord.Types
import System.IO (stderr)

import Lib (request, spawnDataManager)
import MessageHistory (pushMessage, loadHistory, saveHistory)
import Negotiator (startHandler, eventHandler)
import Paths_opinion_bot (getDataFileName)

main :: IO ()
main = do
  tok <- T.readFile =<< getDataFileName ".secrets/auth-token.secret"
  testServerId <- read <$> (readFile =<< getDataFileName ".secrets/guildid.secret")
  history <- loadHistory

  (historyThreadId, reqChannel) <- spawnDataManager pushMessage history

  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = request reqChannel
                                       >>= saveHistory 
                                        >> killThread historyThreadId
                                        >> putStrLn "Ended"
                          , discordOnEvent = eventHandler testServerId reqChannel
                          , discordOnLog = \s -> T.hPutStrLn stderr s >> T.hPutStrLn stderr ""
                          , discordGatewayIntent =
                            def { gatewayIntentMessageContent = True
                                , gatewayIntentMessageChanges = True
                                } -- intents: 33280
                          }
  T.hPutStrLn stderr err
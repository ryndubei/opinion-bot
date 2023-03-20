{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.IO as T
import Discord
import Discord.Types
import Lib (request, spawnDataManager)
import MessageHistory (loadHistory, pushMessage, saveHistory)
import Negotiator (eventHandler, startHandler)
import Paths_opinion_bot (getDataFileName)
import System.IO (stderr)
import Control.Exception (try, IOException)

main :: IO ()
main = do
  tok <- T.readFile =<< getDataFileName ".secrets/auth-token.secret"
  testServerId <- read <$> (readFile =<< getDataFileName ".secrets/guildid.secret")
  history <- loadHistory

  reqChannel <- spawnDataManager pushMessage history

  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = T.hPutStrLn stderr "Ending opinion-bot..."
                                        >> request reqChannel
                                       >>= try . saveHistory
                                       >>= \a -> either (T.hPutStrLn stderr . T.pack . show) pure 
                                                  (a :: Either IOException ())
                                        >> T.hPutStrLn stderr "Ended"
                          , discordOnEvent = eventHandler testServerId reqChannel
                          , discordOnLog = \s -> T.hPutStrLn stderr s >> T.hPutStrLn stderr ""
                          , discordGatewayIntent =
                            def { gatewayIntentMessageContent = True
                                , gatewayIntentMessageChanges = True
                                }
                          }
  T.hPutStrLn stderr err
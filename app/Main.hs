{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Concurrent (Chan, newChan, forkIO, writeChan, killThread, getChanContents)
import qualified Data.Text.IO as T
import Discord
import Discord.Types
import System.IO (stderr)

import Lib (requestFromChan)
import MessageHistory (History, pushMessage, loadHistory, saveHistory)
import Negotiator (startHandler, eventHandler)
import Paths_opinion_bot (getDataFileName)

main :: IO ()
main = do
  tok <- T.readFile =<< getDataFileName ".secrets/auth-token.secret"
  testServerId <- read <$> (readFile =<< getDataFileName ".secrets/guildid.secret")
  history <- loadHistory

  -- If messagesChan receives a Just Message, it updates history, if it receives a 
  -- Nothing, it sends the current history over the history channel.
  -- TODO: move request channel logic over to Lib
  messagesChan <- newChan :: IO (Chan (Maybe Message)) 
  historyChan <- newChan :: IO (Chan History)

  historyThreadId <- forkIO $ getChanContents messagesChan >>= \msgs ->
    iterateHistory historyChan history msgs

  err <- runDiscord $ def { discordToken = tok
                          , discordOnStart = startHandler
                          , discordOnEnd = requestFromChan messagesChan historyChan
                                       >>= saveHistory 
                                        >> killThread historyThreadId
                                        >> putStrLn "Ended"
                          , discordOnEvent = eventHandler testServerId messagesChan historyChan
                          , discordOnLog = \s -> T.hPutStrLn stderr s >> T.hPutStrLn stderr ""
                          , discordGatewayIntent =
                            def { gatewayIntentMessageContent = True
                                , gatewayIntentMessageChanges = True
                                } -- intents: 33280
                          }
  T.hPutStrLn stderr err
  where
    iterateHistory _ _ [] = error "Channel message list should not be empty"
    iterateHistory channel lastHistory (m:ms) = 
      case m of
        Just msg -> iterateHistory channel (pushMessage msg lastHistory) ms
        Nothing -> writeChan channel lastHistory >> iterateHistory channel lastHistory ms

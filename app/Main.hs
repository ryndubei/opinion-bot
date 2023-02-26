{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Exception (try, Exception (displayException), IOException)
import Data.Aeson (encode, eitherDecode)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO as T
import System.Directory (getXdgDirectory, XdgDirectory (XdgCache))
import System.IO (hPutStrLn, stderr)
import Discord
import Discord.Types
import Control.Monad.IO.Class (liftIO)

import MessageHistory (History, emptyHistory)
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

getHistoryLocation :: IO FilePath
getHistoryLocation = getXdgDirectory XdgCache "opinion-bot-history.json"

saveHistory :: History -> IO ()
saveHistory history = getHistoryLocation >>= \path -> 
  BS.writeFile path (encode history)

loadHistory :: IO History
loadHistory = do
  path <- getHistoryLocation
  historyJson <- first (\e -> 
    displayException (e :: IOException)) <$> try (BS.readFile path)
  let mHistory = historyJson >>= eitherDecode
  either 
    (\e -> hPutStrLn stderr e >> pure emptyHistory)
    pure mHistory
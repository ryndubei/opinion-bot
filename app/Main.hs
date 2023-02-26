module Main (main) where

import SentimentAnalysis (analyseRawMessages)

import MessageHistory (History, emptyHistory)

import System.Directory (getXdgDirectory, XdgDirectory (XdgCache))
import Data.Aeson (decode, encode)
import Data.Maybe (fromMaybe)
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = undefined

getHistoryLocation :: IO FilePath
getHistoryLocation = getXdgDirectory XdgCache "opinion-bot-history.json"

saveHistory :: History -> IO ()
saveHistory history = getHistoryLocation >>= \path -> BS.writeFile path (encode history)

loadHistory :: IO History
loadHistory = do
  path <- getHistoryLocation
  mHistory <- decode <$> BS.readFile path
  pure (fromMaybe emptyHistory mHistory) 
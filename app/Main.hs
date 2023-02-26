module Main (main) where

import Control.Exception (try, Exception (displayException), IOException)
import Data.Aeson (encode, eitherDecode)
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BS
import System.Directory (getXdgDirectory, XdgDirectory (XdgCache))
import System.IO (hPutStrLn, stderr)

import MessageHistory (History, emptyHistory)

main :: IO ()
main = undefined

getHistoryLocation :: IO FilePath
getHistoryLocation = getXdgDirectory XdgCache "opinion-bot-history.json"

saveHistory :: History -> IO ()
saveHistory history = getHistoryLocation >>= \path -> BS.writeFile path (encode history)

loadHistory :: IO History
loadHistory = do
  path <- getHistoryLocation
  historyJson <- first (\e -> 
    displayException (e :: IOException)) <$> try (BS.readFile path)
  let mHistory = historyJson >>= eitherDecode
  either 
    (\e -> hPutStrLn stderr e >> pure emptyHistory)
    pure mHistory
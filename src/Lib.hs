module Lib (request, send, spawnDataManager, DataChannel) where

import Control.Concurrent.Chan ( readChan, writeChan, Chan, newChan )
import Control.Concurrent (getChanContents, forkIO)

-- | Two-way channel to a data manager thread. Can either accept new data of
-- type a or be requested to send back b.
newtype DataChannel a b = DataChannel (Chan (Maybe a), Chan b)

-- | Request the current state of b from the data manager.
request :: DataChannel a b -> IO b
request (DataChannel (inChan, outChan)) = do
  writeChan inChan Nothing
  readChan outChan

-- | Send a message to modify the state held by the data manager.
send :: a -> DataChannel a b -> IO ()
send a (DataChannel (inChan, _)) = writeChan inChan (Just a)

-- | Spawn a new manager for DataChannel. Takes data of type a and
-- processes the held state of type b with the given function, and sends
-- back the current state of b when requested.
spawnDataManager :: (a -> b -> b) -> b -> IO (DataChannel a b)
spawnDataManager f s0 = do
  inChan <- newChan :: IO (Chan (Maybe a))
  outChan <- newChan :: IO (Chan b)
  _ <- forkIO $ getChanContents inChan >>= iterateOnInput outChan s0
  pure (DataChannel (inChan, outChan))
  where
    iterateOnInput _ _ [] = error "Channel message list cannot be empty"
    iterateOnInput outChan b (ma:mas) = 
      case ma of
        Just a -> iterateOnInput outChan (f a b) mas
        Nothing -> writeChan outChan b >> iterateOnInput outChan b mas 

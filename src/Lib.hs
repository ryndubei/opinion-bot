module Lib (request, send, spawnRequestManager, RequestChannel) where

import Control.Concurrent.Chan ( readChan, writeChan, Chan, newChan )
import Control.Concurrent (ThreadId, getChanContents, forkIO)

-- TODO: may be a better idea to take over the current thread rather than
-- spawn a new thread

-- | Two-way channel to a request manager thread. Can either accept messages of
-- type a or be requested to send back b.
newtype RequestChannel a b = RequestChannel (Chan (Maybe a), Chan b)

-- | Request the current state of b from the request manager. Will freeze if
-- the request manager thread has been killed.
request :: RequestChannel a b -> IO b
request (RequestChannel (inChan, outChan)) = do
  writeChan inChan Nothing
  readChan outChan

-- | Send a message to modify the state held by the request manager.
send :: a -> RequestChannel a b -> IO ()
send a (RequestChannel (inChan, _)) = writeChan inChan (Just a)

-- | Spawn a new manager for a RequestChannel. Takes messages of type a and
-- processes the held state of type b with the given function, and sends
-- back the current state of b when requested.
spawnRequestManager :: (a -> b -> b) -> b -> IO (ThreadId, RequestChannel a b)
spawnRequestManager f s0 = do
  inChan <- newChan :: IO (Chan (Maybe a))
  outChan <- newChan :: IO (Chan b)
  threadId <- forkIO $ getChanContents inChan >>= iterateOnInput outChan s0
  pure (threadId, RequestChannel (inChan, outChan))
  where
    iterateOnInput _ _ [] = error "Channel message list cannot be empty"
    iterateOnInput outChan b (ma:mas) = 
      case ma of
        Just a -> iterateOnInput outChan (f a b) mas
        Nothing -> writeChan outChan b >> iterateOnInput outChan b mas 

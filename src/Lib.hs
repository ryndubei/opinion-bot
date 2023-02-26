module Lib (requestFromChan) where

import Control.Concurrent.Chan ( readChan, writeChan, Chan )

-- TODO: wrap RequestChannel in a newtype at some point
type RequestChannel a = Chan (Maybe a)

-- | Given a Chan (Maybe a) where Nothing prompts sending
-- data into Chan b, get a new output of b. This assumes
-- that nothing else reads Chan b without this method.
requestFromChan :: RequestChannel a -> Chan b -> IO b
requestFromChan reqChannel outChannel = do
  writeChan reqChannel Nothing
  readChan outChannel
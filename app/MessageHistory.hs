module MessageHistory (deserializeHistory, cacheMessages) where

import Discord.Types (Message (messageId, messageContent, messageAuthor), User, MessageId, ChannelId)
import Data.Map (Map)
import qualified Data.Map.Strict as M
import Data.Text (Text)
import Data.Maybe (fromMaybe, listToMaybe)

-- | Map of channel ids to tuples of the latest recorded message
-- id in the channel and a map of users to lists of the contents
-- of their messages.
type MessageMap = Map ChannelId (Maybe MessageId, Map User [Text])

-- | Type alias for a map of channel ids to Message lists. The message lists
-- are assumed to be ordered.
type ChannelHistory = Map ChannelId [Message]

deserializeHistory :: ChannelHistory -> MessageMap
deserializeHistory = M.map deserializeMessages

deserializeMessages :: [Message] -> (Maybe MessageId, Map User [Text])
deserializeMessages messages = (mId, foldedUserMap)
  where
    mId = messageId <$> listToMaybe messages
    foldedUserMap = foldr 
      (\msg userMap -> 
        let usr = messageAuthor msg
            txt = messageContent msg
         in M.insertWith (++) usr [txt] userMap) M.empty messages

cacheMessages = undefined
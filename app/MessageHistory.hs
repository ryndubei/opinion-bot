module MessageHistory (History, pushMessage, fetchMessages, latestMessageId, oldestMessageId) where

import Data.Map (Map)
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Discord.Types
  ( ChannelId
  , Message (messageAuthor, messageChannelId, messageContent, messageId, messageTimestamp)
  , MessageId
  , User
  )

-- | A store of messages in channels by users who posted them.
newtype History = History (Map ChannelId ChannelMessages)

-- We only really care about the metadata of the latest/oldest recorded
-- message, for the rest of the mesages just the contents are enough.
data ChannelMessages = ChannelMessages
  -- using Map MessageId Text allows us to replace duplicate messages easily
  { userMap :: Map User (Map MessageId Text)
  , latestMessage :: Maybe Message
  , oldestMessage :: Maybe Message
  }

defaultChannelMessages :: ChannelMessages
defaultChannelMessages =
  ChannelMessages
    { userMap = M.empty
    , latestMessage = Nothing
    , oldestMessage = Nothing
    }

-- | Given any Message, record it in the history. Will update duplicate
-- message IDs with new content if edited.
pushMessage :: Message -> History -> History
pushMessage msg (History history) = History $ M.insert (messageChannelId msg) channelMessages' history
  where
    channelMessages = fromMaybe defaultChannelMessages (messageChannelId msg `M.lookup` history)
    channelMessages' =
      ChannelMessages
        { latestMessage =
            case latestMessage channelMessages of
              Nothing -> Just msg
              Just msg' ->
                if messageTimestamp msg > messageTimestamp msg'
                  then Just msg
                  else Just msg'
        , oldestMessage =
            case oldestMessage channelMessages of
              Nothing -> Just msg
              Just msg' ->
                if messageTimestamp msg < messageTimestamp msg'
                  then Just msg
                  else Just msg'
        , userMap =
            M.insertWith
              ( \newMsg oldMsgs ->
                  let [(msgId, msgContent)] = M.toList newMsg -- I am well aware that this is cursed, but I could not find a Data.Map function
                   in M.insert msgId msgContent oldMsgs -- with signature :: (a -> b -> b) -> k -> a -> Map k b -> Map k b
              )
              (messageAuthor msg)
              (M.fromList [(messageId msg, messageContent msg)])
              (userMap channelMessages)
        }

-- | Given a History, fetch all messages that match the optional parameters for
-- Channel and User.
fetchMessages :: History -> Maybe ChannelId -> Maybe User -> [Text]
fetchMessages history@(History historyMap) mcid muser =
  case mcid of
    Just cid ->
      let cmsgs = fetchChannelMessages history cid
       in case muser of
            Just user -> fetchUserMessages cmsgs user
            Nothing -> concatMap (fetchUserMessages cmsgs) (M.keys (userMap cmsgs))
    Nothing -> concatMap (\cid -> fetchMessages history (Just cid) muser) (M.keys historyMap)

fetchChannelMessages :: History -> ChannelId -> ChannelMessages
fetchChannelMessages (History history) cid = fromMaybe defaultChannelMessages $ cid `M.lookup` history

fetchUserMessages :: ChannelMessages -> User -> [Text]
fetchUserMessages cmsgs user = maybe [] (map snd . M.toList) (user `M.lookup` userMap cmsgs)

-- | Get the MessageId of the latest message posted to a channel.
latestMessageId :: History -> ChannelId -> Maybe MessageId
latestMessageId history cid = messageId <$> (latestMessage . fetchChannelMessages history) cid

-- | Get the MessageId of the oldest message posted to a channel.
oldestMessageId :: History -> ChannelId -> Maybe MessageId
oldestMessageId history cid = messageId <$> (oldestMessage . fetchChannelMessages history) cid

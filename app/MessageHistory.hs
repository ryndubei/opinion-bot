{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module MessageHistory (History, pushMessage, fetchMessages, latestMessageId, oldestMessageId) where

import Data.Aeson

import Data.Map (Map)
import qualified Data.Map.Strict as M

import Data.Text (Text)

import Discord.Types
  ( ChannelId
  , Message (messageAuthor, messageChannelId, messageContent, messageId, messageTimestamp)
  , MessageId
  , User
  )

import Data.Bifunctor (second)
import Data.Maybe (fromMaybe)

-- | A store of messages in channels by users who posted them.
newtype History = History (Map ChannelId ChannelMessages) deriving (Show)

-- We only really care about the metadata of the latest/oldest recorded
-- message, for the rest of the mesages just the contents are enough.
data ChannelMessages = ChannelMessages
  -- using Map MessageId Text allows us to replace duplicate messages easily
  { userMap :: Map User (Map MessageId Text)
  , latestMessage :: Maybe Message
  , oldestMessage :: Maybe Message
  } deriving (Show)

instance FromJSON History where
  parseJSON = \case
    Object o -> (o .: "History") >>= fmap (History . M.fromList) . parseJSON
    x -> fail $ "unexpected json: " ++ show x

instance ToJSON History where
  toJSON (History historyMap) = Object ("History" .= M.toList historyMap)

instance FromJSON ChannelMessages where
  parseJSON = withObject "ChannelMessages" $ \v -> ChannelMessages
    -- I cannot convert directly because User and MessageId have no FromJSONKey
    -- instance, so I have to turn the maps into a list first.
    <$> fmap (M.fromList . map (second M.fromList)) (v .: "userMap")
    <*> v .: "latestMessage"
    <*> v .: "oldestMessage"

instance ToJSON ChannelMessages where
  toJSON (ChannelMessages userMap latestMessage oldestMessage) =
    object 
      [ "userMap" .= (map (second M.toList) . M.toList) userMap
      , "latestMessage" .= latestMessage
      , "oldestMessage" .= oldestMessage
      ]
  toEncoding (ChannelMessages userMap latestMessage oldestMessage) =
    pairs ( "userMap" .= (map (second M.toList) . M.toList) userMap 
         <> "latestMessage" .= latestMessage 
         <> "oldestMessage" .= oldestMessage)

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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module MessageHistory 
  ( History 
  , pushMessage
  , fetchMessages 
  , latestMessageId 
  , oldestMessageId 
  , saveHistory
  , loadHistory
  , fetchChannelIds
  , fetchUserIds
  ) 
where

import Control.Exception
import Data.Aeson
import Data.Bifunctor (first, second)
import qualified Data.ByteString.Lazy as BS
import Data.Map (Map)
import qualified Data.Map.Strict as M
import qualified Data.Map.Merge.Strict as M
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Discord.Types
  ( ChannelId
  , Message (messageAuthor, messageChannelId, messageContent, messageId, messageTimestamp)
  , MessageId
  , userId
  , UserId, UTCTime
  )
import System.Directory (getXdgDirectory, XdgDirectory(XdgCache), createDirectoryIfMissing )
import System.IO (hPutStrLn, stderr)
import System.FilePath (dropFileName)

-- | A store of messages in channels by users who posted them.
newtype History = History (Map ChannelId ChannelMessages) deriving (Show)

emptyHistory :: History
emptyHistory = History M.empty

getHistoryLocation :: IO FilePath
getHistoryLocation = getXdgDirectory XdgCache "opinion-bot-history.json"

saveHistory :: History -> IO ()
saveHistory history = getHistoryLocation >>= \path ->
  createDirectoryIfMissing True (dropFileName path) 
  >> BS.writeFile path (encode history)

loadHistory :: IO History
loadHistory = do
  path <- getHistoryLocation
  historyJson <- first (\e -> 
    displayException (e :: IOException)) <$> try (BS.readFile path)
  let mHistory = historyJson >>= eitherDecode
  either 
    (\e -> hPutStrLn stderr e >> pure emptyHistory)
    pure mHistory

-- We only really care about the metadata of the latest/oldest recorded
-- message, for the rest of the mesages just the contents are enough.
data ChannelMessages = ChannelMessages
  -- using Map MessageId Text allows us to replace duplicate messages easily
  { userMap :: Map UserId (Map MessageId Text)
  , latestMessage :: Maybe (UTCTime, MessageId)
  , oldestMessage :: Maybe (UTCTime, MessageId)
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
              Nothing -> Just (messageTimestamp msg, messageId msg)
              Just (existingTime, existingId) ->
                if messageTimestamp msg > existingTime
                  then Just (messageTimestamp msg, messageId msg)
                  else Just (existingTime, existingId)

        , oldestMessage =
            case oldestMessage channelMessages of
              Nothing -> Just (messageTimestamp msg, messageId msg)
              Just (existingTime, existingId) ->
                if messageTimestamp msg < existingTime
                  then Just (messageTimestamp msg, messageId msg)
                  else Just (existingTime, existingId)

        , userMap =
            M.insertWith
              combine
              ((userId . messageAuthor) msg)
              (M.fromList [(messageId msg, messageContent msg)])
              (userMap channelMessages)
        }

-- | Combine the elements of two Maps, giving priority to values of the first
-- map whenever there are two identical keys.
combine :: Ord k => Map k v -> Map k v -> Map k v
combine = M.merge M.preserveMissing' M.preserveMissing' (M.zipWithMatched (\_k x _y -> x))

-- | Given a History, fetch all messages that match the optional parameters for
-- Channel and User.
fetchMessages :: History -> Maybe ChannelId -> Maybe UserId -> [Text]
fetchMessages history@(History historyMap) mcid muser =
  case mcid of
    Just cid ->
      let cmsgs = fetchChannelMessages history cid
       in case muser of
            Just user -> fetchUserMessages cmsgs user
            -- if we don't have a specific user id we fetch messages from all users
            Nothing -> concatMap (fetchUserMessages cmsgs) (M.keys (userMap cmsgs))
    -- if we don't have a specific channel id we fetch messages from all channel ids
    Nothing -> concatMap (\cid -> fetchMessages history (Just cid) muser) (M.keys historyMap)

-- | Given a History, fetch all UserIds recorded there, optionally in a particular channel.
fetchUserIds :: History -> Maybe ChannelId -> [UserId]
fetchUserIds history@(History historyMap) mcid =
  case mcid of
    Just cid ->
      let cmsgs = fetchChannelMessages history cid
       in M.keys (userMap cmsgs)
    Nothing -> concatMap (M.keys . userMap . fetchChannelMessages history) (M.keys historyMap)

-- | Given a History, fetch all ChannelIds recorded there, optionally just those containing 
-- messages from a particular user.
fetchChannelIds :: History -> Maybe UserId -> [ChannelId]
fetchChannelIds history@(History historyMap) muser =
  case muser of
    Just user -> filter (elem user . M.keys . userMap . fetchChannelMessages history)
                        . M.keys $ historyMap
    Nothing -> M.keys historyMap

fetchChannelMessages :: History -> ChannelId -> ChannelMessages
fetchChannelMessages (History history) cid = fromMaybe defaultChannelMessages $ cid `M.lookup` history

fetchUserMessages :: ChannelMessages -> UserId -> [Text]
fetchUserMessages cmsgs user = maybe [] (map snd . M.toList) (user `M.lookup` userMap cmsgs)

-- | Get the MessageId of the latest message posted to a channel.
latestMessageId :: History -> ChannelId -> Maybe MessageId
latestMessageId history cid = snd <$> (latestMessage . fetchChannelMessages history) cid

-- | Get the MessageId of the oldest message posted to a channel.
oldestMessageId :: History -> ChannelId -> Maybe MessageId
oldestMessageId history cid = snd <$> (oldestMessage . fetchChannelMessages history) cid

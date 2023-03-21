{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
{-# LANGUAGE TupleSections #-}
module Commands (mySlashCommands, SlashCommand (..)) where

import Control.Monad (void, when, forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, sortOn)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Interactions
import Discord.Requests (MessageTiming (BeforeMessage, LatestMessages))
import qualified Discord.Requests as R
import Discord.Types
import Lib (DataChannel, request, send)
import MessageHistory (History, fetchMessages, oldestMessageId, fetchUserIds)
import Numeric (showFFloat)
import SentimentAnalysis (analyseRawMessages, wordInvariant, analyseRawMessage)
import SentimentAnalysis.SentimentData (getSentimentData)
import Utils
import System.Random ( StdGen, randomR, randomIO, mkStdGen)

data SlashCommand = SlashCommand
  { name :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()}

mySlashCommands :: [DataChannel Message History -> SlashCommand]
mySlashCommands = [importData, analyse, something, top10]

importData :: DataChannel Message History -> SlashCommand
importData msgChannel = SlashCommand
  { name = "import"
  , registration = createChatInput "import" "import old messages from a channel"
  , handler = \intr _options -> do
      let cid = interactionChannelId intr
      when (isNothing cid) $ fail "Could not get origin channel of slash command"
      void . restCall . ephermeralInteractionResponse intr $
        ("Importing messages from " <> displayChannel (fromJust cid))
      targetMessageTiming <- maybe LatestMessages BeforeMessage . (`oldestMessageId` fromJust cid)
        <$> (liftIO . request) msgChannel
      report <- runImport targetMessageTiming msgChannel (fromJust cid)
      echo report
      void . restCall $
        R.EditOriginalInteractionResponse
          (interactionApplicationId intr)
          (interactionToken intr)
          (ephermeralMessage report)
  }

runImport :: MessageTiming -> DataChannel Message History -> ChannelId -> DiscordHandler Text
runImport msgTiming msgChannel cid = do
  echo ("Importing messages: " <> (T.pack . show) msgTiming)
  mmsgs <- restCall $ R.GetChannelMessages cid (100, msgTiming)
  case mmsgs of
    Left e -> (pure . T.pack) ("Error while importing messages: " ++ show e)
    Right msgs ->
      if null msgs
        then pure ("Done importing messages from " <> displayChannel cid )
        else do
          forM_ msgs $ \msg ->
            unless (userIsBot (messageAuthor msg) || userIsWebhook (messageAuthor msg)) 
              $ liftIO (send msg msgChannel)
          oldestMsg <- fromMaybe (error "Oldest message ID should exist at this point")
            . (`oldestMessageId` cid) <$> (liftIO . request) msgChannel
          runImport (BeforeMessage oldestMsg) msgChannel cid

analyse :: DataChannel Message History -> SlashCommand
analyse msgChannel = SlashCommand
  { name = "analyse"
  , registration = createChatInput "analyse" "analyse message sentiment"
      >>= \cac -> pure $ cac { createOptions =
        Just $ OptionsValues [ OptionValueString "keyword" Nothing "keyword for sentiment analysis" Nothing True (Left False) (Just 1) Nothing
                             , OptionValueChannel "channel" Nothing "text channel to analyse" Nothing False (Just [ApplicationCommandChannelTypeGuildText])
                             , OptionValueUser "user" Nothing "user to analyse" Nothing False
                             ]
        }
  , handler = \intr options -> liftIO (request msgChannel) >>= \history ->
      let options' = unwrapOptions options
       in case options' of
            Right (keyword, cid, uid) -> handleOptions intr history (keyword, cid, uid)
            Left err -> echo err
  }
  where
    unwrapOptions :: Maybe OptionsData -> Either Text (Text, Maybe ChannelId, Maybe UserId)
    unwrapOptions (Just (OptionsDataValues options)) =
      let mkeyword = optionDataValueString <$> find (\o -> optionDataValueName o == "keyword") options
          mcid = optionDataValueChannel <$> find (\o -> optionDataValueName o == "channel") options
          muid = optionDataValueUser <$> find (\o -> optionDataValueName o == "user") options
       in case mkeyword of
        Just (Right keyword) -> pure (keyword, mcid, muid)
        _ -> Left "did not receive keyword for analyse"
    unwrapOptions _ = Left "did not receive appropriate options for analyse"

    handleOptions :: Interaction -> History -> (Text, Maybe ChannelId, Maybe UserId) -> DiscordHandler ()
    handleOptions intr history (keyword, cid, uid) =
      let txts = fetchMessages history cid uid
          reply x = "Calculated sentiment for keyword "
            <> keyword
            <> (if isJust cid then " in channel " <> displayChannel (fromJust cid) else "")
            <> (if isJust uid then " by user " <> displayUser (fromJust uid) else "")
            <> ": "
            <> T.pack (showFFloat (Just 6) x "")
       in case wordInvariant keyword of
            Right _ -> do
              analyseMessages <- analyseRawMessages <$> liftIO getSentimentData
              let sentiment = analyseMessages txts keyword
              void . restCall $ standardInteractionResponse intr (reply sentiment)
            Left err -> void . restCall $ ephermeralInteractionResponse intr ("Invalid input: " <> err)

something :: DataChannel Message History -> SlashCommand
-- TODO: wrap DataChannel Message History as part of some Record, so that more slash command
-- creator parameters can be added
something msgChannel = SlashCommand
  { name = "something"
  , registration = createChatInput "something" "says something"
  , handler = \intr _options -> liftIO (request msgChannel) >>= \history -> do
      stdGen <- mkStdGen <$> liftIO randomIO -- TODO: maybe use a DataChannel that manages StdGen
      let txt = getRandomMessage history stdGen
      void . restCall $ standardInteractionResponse intr txt
  }
  where
    getRandomMessage :: History -> StdGen -> Text
    getRandomMessage history stdgen =
      let txts = fetchMessages history Nothing Nothing
          txt = txts!!fst (randomR (0,length txts - 1) stdgen)
       in if null txts
        then "I don't have anything to say"
        else txt

top10 :: DataChannel Message History -> SlashCommand
top10 msgChannel = SlashCommand
  { name = "topten"
  , registration = createChatInput "topten" "top 10 best messages on the server"
  , handler = \intr _options -> liftIO (request msgChannel) >>= \history -> do
      analyseMessage <- analyseRawMessage <$> liftIO getSentimentData
      void . restCall $ ephermeralInteractionResponse intr "Calculating top 10 messages..."
      let users = fetchUserIds history Nothing
          userTxts = map (\u -> (u, fetchMessages history Nothing (Just u))) users
          userTxts' = concatMap (\(u, ts) -> map (u,) ts) userTxts
          top10Messages = take 10 $ sortOn (analyseMessage . snd) userTxts'
          replies = zipWith ((<>) . (<> ". ") . T.pack . show)
                                      ([1..] :: [Int])
                                      (map (\(u, t) -> displayUser u <> ": " <> t) top10Messages)
          replies' = map (T.take 2000) replies
      void . restCall $ followup intr "Top 10 messages:"
      forM_ replies' $ \reply -> do
        err <- restCall $ followup intr reply
        echo ((T.pack . show) err)
  }
  where
    followup intr reply = 
      R.CreateFollowupInteractionMessage 
        (interactionApplicationId intr) 
        (interactionToken intr) 
        (standardMessage reply)
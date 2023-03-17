{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Commands (mySlashCommands, SlashCommand (..)) where

import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Maybe (fromJust, fromMaybe, isJust, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Interactions
import Discord.Requests (MessageTiming (BeforeMessage, LatestMessages))
import qualified Discord.Requests as R
import Discord.Types
import Lib (DataChannel, request, send)
import MessageHistory (History, fetchMessages, oldestMessageId)
import Numeric (showFFloat)
import SentimentAnalysis (analyseRawMessages, wordInvariant)
import Utils

data SlashCommand = SlashCommand
  { name :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()}

mySlashCommands :: [DataChannel Message History -> SlashCommand]
mySlashCommands = [ping, importData, analyse]

ping :: DataChannel Message History -> SlashCommand
ping _ = SlashCommand
  { name = "ping"
  , registration = createChatInput "ping" "responds pong"
  , handler = \intr _options -> void . restCall $ standardInteractionResponse intr "pong"
  }

importData :: DataChannel Message History -> SlashCommand
importData msgChannel = SlashCommand
  { name = "import"
  , registration = createChatInput "import" "import old messages from a channel"
  , handler = \intr _options -> do
      let cid = interactionChannelId intr
      when (isNothing cid) $ fail "Could not get origin channel of slash command"
      void . restCall . standardInteractionResponse intr $
        ("Importing messages from " <> displayChannel (fromJust cid))
      targetMessageTiming <- maybe LatestMessages BeforeMessage . (`oldestMessageId` fromJust cid)
        <$> (liftIO . request) msgChannel
      report <- runImport targetMessageTiming msgChannel (fromJust cid)
      echo report
      void . restCall $
        R.EditOriginalInteractionResponse
          (interactionApplicationId intr)
          (interactionToken intr)
          (interactionResponseMessageBasic report)
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
          mapM_ (liftIO . (`send` msgChannel)) msgs
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
    unwrapOptions (Just (OptionsDataValues options)) = traceShow options $
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
            Right _ -> liftIO (analyseRawMessages txts keyword) >>= \sentiment ->
              void . restCall $ standardInteractionResponse intr (reply sentiment)
            Left err -> void . restCall $ standardInteractionResponse intr ("Invalid input: " <> err)

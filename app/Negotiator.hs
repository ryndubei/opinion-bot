{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module Negotiator (startHandler, eventHandler) where

import Data.Text (Text)
import Discord.Interactions
import Discord
import qualified Discord.Requests as R
import Control.Monad (void, when, forM_)
import MessageHistory (History, oldestMessageId, fetchMessages)
import Lib (DataChannel, send, request)
import qualified Data.Text.IO as T
import System.IO (stderr)
import Control.Monad.IO.Class (liftIO)
import Discord.Types
import Data.Maybe (isNothing, fromJust, fromMaybe, isJust)
import qualified Data.Text as T
import Discord.Requests (MessageTiming (LatestMessages, BeforeMessage))
import Data.List (find)
import SentimentAnalysis (analyseRawMessages, wordInvariant)
import Numeric (showFFloat)

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
  , handler = \intr _options ->
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic "pong")
  }

importData :: DataChannel Message History -> SlashCommand
importData msgChannel = SlashCommand
  { name = "import"
  , registration = createChatInput "import" "import old messages from a channel"
  , handler = \intr _options -> do
      let cid = interactionChannelId intr
      when (isNothing cid) $ fail "Could not get origin channel of slash command"
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic $ "Importing messages from " <> displayChannel (fromJust cid))
      msgTiming <- maybe LatestMessages BeforeMessage . (`oldestMessageId` fromJust cid)
        <$> (liftIO . request) msgChannel
      report <- runImport msgTiming msgChannel (fromJust cid)
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

displayChannel :: ChannelId -> Text
displayChannel cid = T.pack ("<#" ++ show cid ++ ">")

displayUser :: UserId -> Text
displayUser uid = T.pack ("<@" ++ show uid ++ ">")

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

  , handler = \intr options -> do
      history <- liftIO $ request msgChannel
      case options of
        Just (OptionsDataValues ( OptionDataValueString {optionDataValueString = Right keyword}
                                : os
                                )) ->
          let (cid,uid) = case os of
                [OptionDataValueChannel {optionDataValueChannel = c}, OptionDataValueUser {optionDataValueUser = u}] -> (Just c, Just u)
                [OptionDataValueChannel {optionDataValueChannel = c}, _] -> (Just c, Nothing)
                [OptionDataValueUser {optionDataValueUser = u}] -> (Nothing, Just u)
                _ -> (Nothing,Nothing)
              txts = fetchMessages history cid uid
              reply = "Calculated sentiment for keyword " <> keyword 
                <> (if isJust cid then " in channel " <> displayChannel (fromJust cid) else "")
                <> (if isJust uid then " by user " <> displayUser (fromJust uid) else "")
                <> ": "
           in if wordInvariant keyword
                then liftIO (analyseRawMessages txts keyword)
                  >>= \sentiment -> void . restCall $
                    R.CreateInteractionResponse
                      (interactionId intr)
                      (interactionToken intr)
                      (InteractionResponseChannelMessage $
                        (interactionResponseMessageBasic (reply <> T.pack (showFFloat (Just 6) sentiment ""))) 
                          { interactionResponseMessageAllowedMentions = Just (def {mentionUsers = False}) }
                      )
                else void . restCall $ R.CreateInteractionResponse
                  (interactionId intr)
                  (interactionToken intr)
                  (interactionResponseBasic 
                    "Invalid input: keyword must be letters only without spaces"
                  )
        _ -> echo "did not receive keyword for analyse"
  }

startHandler :: DiscordHandler ()
startHandler = echo "Started opinion-bot"

eventHandler :: GuildId -> DataChannel Message History -> Event -> DiscordHandler ()
eventHandler testServerId msgChannel = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady mySlashCommands' appId testServerId
  InteractionCreate intr                         -> onInteractionCreate mySlashCommands' intr
  MessageCreate msg                              -> echo ( "( "
                                                        <> (T.pack . show . messageTimestamp $ msg)
                                                        <> " ) "
                                                        <> (userName . messageAuthor) msg
                                                        <> ": "
                                                        <> messageContent msg
                                                        <> if (not . null . messageAttachments) msg then " [ATTACHMENT]" else ""
                                                        <> if (not . null . messageEmbeds) msg then " [EMBED]" else ""
                                                         )
                                                      >> liftIO (send msg msgChannel)
  _                                              -> pure ()
  where
    mySlashCommands' = map (\f -> f msgChannel) mySlashCommands

onReady :: [SlashCommand] -> ApplicationId -> GuildId -> DiscordHandler ()
onReady slashCmds appId testServerId = do
  echo "Bot ready!"

  appCmdRegistrations <- mapM tryRegistering slashCmds

  case sequence appCmdRegistrations of
    Left err ->
      echo $ "[!] Failed to register some commands" <> (T.pack . show) err

    Right cmds -> do
      echo $ "Registered " <> (T.pack . show) (length cmds) <> " command(s)."
      unregisterOutdatedCmds cmds
  where
  tryRegistering cmd = case registration cmd of
    Just reg -> restCall $ R.CreateGuildApplicationCommand appId testServerId reg
    Nothing -> pure . Left $ RestCallErrorCode 0 "" ""

  unregisterOutdatedCmds validCmds = do
    registered <- restCall $ R.GetGuildApplicationCommands appId testServerId
    case registered of
      Left err ->
        echo $ "Failed to get registered slash commands: " <> (T.pack . show) err

      Right cmds ->
        let validIds = map applicationCommandId validCmds
            outdatedIds = filter (`notElem` validIds)
                          . map applicationCommandId
                          $ cmds
         in forM_ outdatedIds $
              restCall . R.DeleteGuildApplicationCommand appId testServerId

echo :: Text -> DiscordHandler ()
echo t = liftIO $ T.hPutStrLn stderr t

onInteractionCreate :: [SlashCommand] -> Interaction -> DiscordHandler ()
onInteractionCreate slashCmds = \case
  cmd@InteractionApplicationCommand
    { applicationCommandData = input@ApplicationCommandDataChatInput {} } ->
      case
        find (\c -> applicationCommandDataName input == name c) slashCmds
      of
        Just found ->
          handler found cmd (optionsData input)
        Nothing ->
          echo "Somehow got unknown slash command (registrations out of date?)"
  _ -> pure () -- Unexpected/unsupported interaction type
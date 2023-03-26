{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Negotiator (startHandler, eventHandler) where

import Commands
import Control.Monad (forM_, unless, void)
import Control.Monad.IO.Class (liftIO)
import Data.List (find, maximumBy)
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import Lib (send, request)
import Utils
import Data.Bool (bool)
import MessageHistory (fetchMessages, fetchChannelIds)
import TextSimilarity (findReply)
import Data.Ord (comparing)
import Data.Maybe (mapMaybe)

startHandler :: DiscordHandler ()
startHandler = echo "Started opinion-bot"

eventHandler :: GuildId -> Constants -> Event -> DiscordHandler ()
eventHandler testServerId constants@Constants{msgChannel, chatbotMode} = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady mySlashCommands' appId testServerId
  InteractionCreate intr                         -> onInteractionCreate mySlashCommands' intr
  MessageCreate msg                              -> echo (displayMessage msg)
                                                    >> liftIO (request chatbotMode)
                                                    >>= bool (pure ()) (chatbotRespond msg)
                                                    >> sendMessage msg
  MessageUpdate cid msgid                        -> restCall (R.GetChannelMessage (cid,msgid))
                                                      >>= \case
                                                        Right msg -> do
                                                          echo ("Message edited in channel "
                                                              <> displayChannel cid
                                                              <> ":\n\t"
                                                              <> displayMessage msg)
                                                          sendMessage msg
                                                        Left err -> echo $
                                                          "Failed to get message with id "
                                                          <> T.pack (show msgid)
                                                          <> " from channel "
                                                          <> displayChannel cid
                                                          <> ": "
                                                          <> T.pack (show err)
  _                                              -> pure ()
  where
    mySlashCommands' = map (\f -> f constants) mySlashCommands
    sendMessage msg = liftIO (unless (shouldIgnore msg)
                             (send msg msgChannel))
    shouldIgnore msg = userIsBot (messageAuthor msg)
      || userIsWebhook (messageAuthor msg)
      || T.null (messageContent msg)
    -- Responds to messages with the most fitting response from the message history.
    chatbotRespond :: Message -> DiscordHandler ()
    chatbotRespond msg = unless (shouldIgnore msg) $ do
      history <- liftIO (request msgChannel)
      let cids = fetchChannelIds history Nothing
          messages = map (\cid -> fetchMessages history (Just cid) Nothing) cids
          messages' = mapMaybe (`findReply` messageContent msg) messages
          (_, reply, confidence) = if not (null messages')
            then maximumBy (comparing (\(_,_,x) -> x)) messages'
            else ("", "I don't have anything to say.", 0)
      echo ("Chatbot responding to message: " <> messageContent msg
            <> "\nResponse candidates:\n\t" <> T.intercalate "\n\t" (map (T.pack . show) messages')
            <> "\nChatbot responding with confidence: " <> T.pack (show confidence))
      void . restCall $ R.CreateMessage (messageChannelId msg) reply

displayMessage :: Message -> Text
displayMessage msg = "( "
                  <> (T.pack . show . messageTimestamp $ msg)
                  <> " ) "
                  <> (userName . messageAuthor) msg
                  <> ": "
                  <> messageContent msg
                  <> if (not . null . messageAttachments) msg then " [ATTACHMENT]" else ""
                  <> if (not . null . messageEmbeds) msg then " [EMBED]" else ""

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
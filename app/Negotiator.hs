{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Negotiator (startHandler, eventHandler) where

import Commands
import Control.Monad (forM_, unless)
import Control.Monad.IO.Class (liftIO)
import Data.List (find)
import Data.Text (Text)
import qualified Data.Text as T
import Discord
import Discord.Interactions
import qualified Discord.Requests as R
import Discord.Types
import Lib (DataChannel, send)
import MessageHistory (History)
import Utils

startHandler :: DiscordHandler ()
startHandler = echo "Started opinion-bot"

eventHandler :: GuildId -> DataChannel Message History -> Event -> DiscordHandler ()
eventHandler testServerId msgChannel = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady mySlashCommands' appId testServerId
  InteractionCreate intr                         -> onInteractionCreate mySlashCommands' intr
  MessageCreate msg                              -> echo (displayMessage msg)
                                                      >> liftIO 
                                                          (unless 
                                                          (userIsBot (messageAuthor msg) || userIsWebhook (messageAuthor msg)) 
                                                          (send msg msgChannel))
  MessageUpdate cid msgid                        -> restCall (R.GetChannelMessage (cid,msgid))
                                                      >>= \case
                                                        Right msg -> do
                                                          echo ("Message edited in channel "
                                                              <> displayChannel cid
                                                              <> ":\n\t"
                                                              <> displayMessage msg)
                                                          liftIO (send msg msgChannel)
                                                        Left err -> echo $
                                                          "Failed to get message with id "
                                                          <> T.pack (show msgid)
                                                          <> " from channel "
                                                          <> displayChannel cid
                                                          <> ": "
                                                          <> T.pack (show err)
  _                                              -> pure ()
  where
    mySlashCommands' = map (\f -> f msgChannel) mySlashCommands

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
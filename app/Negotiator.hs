{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Negotiator (startHandler, eventHandler) where

import Data.Text (Text)
import Discord.Interactions 
import Discord
import qualified Discord.Requests as R
import Control.Monad (void)
import MessageHistory (History)
import Control.Concurrent (Chan)
import qualified Data.Text.IO as T
import System.IO (stderr)
import Control.Monad.IO.Class (liftIO)
import Discord.Types

data SlashCommand = SlashCommand
  { name :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()}

mySlashCommands :: [Chan (Maybe Message) -> Chan History -> SlashCommand]
mySlashCommands = [ping, importData, analyse]

ping :: Chan (Maybe Message) -> Chan History -> SlashCommand
ping _ _ = SlashCommand
  { name = "ping"
  , registration = createChatInput "ping" "responds pong"
  , handler = \intr _options ->
      void . restCall $
        R.CreateInteractionResponse
          (interactionId intr)
          (interactionToken intr)
          (interactionResponseBasic "pong")
  }

importData :: Chan (Maybe Message) -> Chan History -> SlashCommand
importData messageChan _ = undefined

analyse :: Chan (Maybe Message) -> Chan History -> SlashCommand
analyse messageChan historyChan = undefined

startHandler :: DiscordHandler ()
startHandler = liftIO $ T.hPutStrLn stderr "Started opinion-bot"

eventHandler :: GuildId -> Chan (Maybe Message) -> Chan History -> Event -> DiscordHandler ()
eventHandler testServerId messageChan historyChan = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId testServerId
  InteractionCreate intr                         -> onInteractionCreate messageChan historyChan intr
  _                                              -> pure ()

onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady = undefined

onInteractionCreate :: Chan (Maybe Message) -> Chan History -> Interaction -> DiscordHandler ()
onInteractionCreate = undefined
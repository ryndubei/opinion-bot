{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Negotiator (startHandler, eventHandler) where

import Data.Text (Text)
import Discord.Interactions 
import Discord
import qualified Discord.Requests as R
import Control.Monad (void)
import MessageHistory (History)
import Lib (RequestChannel)
import qualified Data.Text.IO as T
import System.IO (stderr)
import Control.Monad.IO.Class (liftIO)
import Discord.Types

data SlashCommand = SlashCommand
  { name :: Text
  , registration :: Maybe CreateApplicationCommand
  , handler :: Interaction -> Maybe OptionsData -> DiscordHandler ()}

mySlashCommands :: [RequestChannel Message History -> SlashCommand]
mySlashCommands = [ping, importData, analyse]

ping :: RequestChannel Message History -> SlashCommand
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

importData :: RequestChannel Message History -> SlashCommand
importData reqChannel = undefined

analyse :: RequestChannel Message History -> SlashCommand
analyse reqChannel = undefined

startHandler :: DiscordHandler ()
startHandler = liftIO $ T.hPutStrLn stderr "Started opinion-bot"

eventHandler :: GuildId -> RequestChannel Message History -> Event -> DiscordHandler ()
eventHandler testServerId reqChannel = \case
  Ready _ _ _ _ _ _ (PartialApplication appId _) -> onReady appId testServerId
  InteractionCreate intr                         -> onInteractionCreate reqChannel intr
  _                                              -> pure ()

onReady :: ApplicationId -> GuildId -> DiscordHandler ()
onReady = undefined

onInteractionCreate :: RequestChannel Message History -> Interaction -> DiscordHandler ()
onInteractionCreate = undefined
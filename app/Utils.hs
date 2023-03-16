{-# OPTIONS_GHC -Wno-missing-export-lists #-}
module Utils where

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Discord
import Discord.Interactions
import Discord.Requests (InteractionResponseRequest)
import qualified Discord.Requests as R
import Discord.Types
import System.IO (stderr)

echo :: Text -> DiscordHandler ()
echo t = liftIO $ T.hPutStrLn stderr t

displayChannel :: ChannelId -> Text
displayChannel cid = T.pack ("<#" ++ show cid ++ ">")

displayUser :: UserId -> Text
displayUser uid = T.pack ("<@" ++ show uid ++ ">")

standardInteractionResponse :: Interaction -> Text -> InteractionResponseRequest ()
standardInteractionResponse intr reply =
  R.CreateInteractionResponse
    (interactionId intr)
    (interactionToken intr)
    (InteractionResponseChannelMessage $
      (interactionResponseMessageBasic reply)
        { interactionResponseMessageAllowedMentions = Just (def {mentionEveryone = False, mentionUsers = False})}
    )

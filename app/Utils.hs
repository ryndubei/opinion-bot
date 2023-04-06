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
import System.Random (StdGen, randomR)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import Data.Bifunctor (second)

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
    (InteractionResponseChannelMessage (standardMessage reply))

standardMessage :: Text -> InteractionResponseMessage
standardMessage reply = (interactionResponseMessageBasic reply)
  { interactionResponseMessageAllowedMentions =
    Just (def {mentionEveryone = False, mentionUsers = False})}

ephermeralInteractionResponse :: Interaction -> Text -> InteractionResponseRequest ()
ephermeralInteractionResponse intr reply =
  R.CreateInteractionResponse
    (interactionId intr)
    (interactionToken intr)
    (InteractionResponseChannelMessage (ephermeralMessage reply))

ephermeralMessage :: Text -> InteractionResponseMessage
ephermeralMessage reply = (interactionResponseMessageBasic reply)
  { interactionResponseMessageFlags =
    Just (InteractionResponseMessageFlags [InteractionResponseMessageFlagEphermeral])}

-- | Draw a random element from a list, weighted by the second element of the tuple.
-- Higher weights are more likely to be drawn.
-- Negative or zero weights will never be drawn, unless all weights are negative or zero.
drawWeightedRandom :: StdGen -> NonEmpty (a, Double) -> a
drawWeightedRandom stdGen xs =
  let xs' = fmap (second (`max` 0)) xs
      xs'' = NE.zip (fmap fst xs') (NE.scanl1 (+) (fmap snd xs'))
      total = snd (NE.last xs'')
      (roll, _) = randomR (0, total) stdGen
      x = fst . head $ NE.dropWhile ((< roll) . snd) xs''
   in if total == 0
    then fst $ NE.head (NE.sortBy (\(_,a) (_,b) -> compare a b) xs)
    else x

timestamp :: MessageId -> UTCTime
timestamp = snowflakeCreationDate . unId
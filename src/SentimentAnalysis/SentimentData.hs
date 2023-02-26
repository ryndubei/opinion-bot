{-# LANGUAGE TupleSections #-}
module SentimentAnalysis.SentimentData (SentimentData, getSentimentData, lookupSentiment) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Data.Map (Map)
import qualified Data.Map.Strict as M

import Paths_opinion_bot ( getDataFileName )

newtype SentimentData = SentimentData (Map Text Bool)

-- | Use the `positive-words.txt` and `negative-words.txt` files
-- to generate a dictionary of words mapped to Doubles.
getSentimentData :: IO SentimentData
getSentimentData = do
  positiveWords <- drop 35 . T.lines <$> (T.readFile =<< getDataFileName "positive-words.txt")
  negativeWords <- drop 35 . T.lines <$> (T.readFile =<< getDataFileName "negative-words.txt")
  let pairs = map (,True) positiveWords ++ map (,False) negativeWords
  pure (SentimentData (M.fromList pairs))

-- | Access SentimentData for a certain word, returning 1.0 if the word
-- is positive, -1.0 if the word is negative, and 0 if the word is neutral.
lookupSentiment :: Text -> SentimentData -> Double
lookupSentiment word (SentimentData sentiments) = 
  case word `M.lookup` sentiments of
    Just True -> 1.0
    Just False -> (-1.0)
    Nothing -> 0

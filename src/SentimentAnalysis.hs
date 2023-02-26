{-# LANGUAGE TupleSections #-}
module SentimentAnalysis ( analyseRawMessages ) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Data.Char (isLetter)
import Data.Maybe (mapMaybe)

import Paths_opinion_bot ( getDataFileName )

-- | Given an unprocessed list of Texts and a target word, find the
-- average sentiment of the texts containing that word as a value
-- between -1 and 1.
analyseRawMessages :: [Text] -> Text -> IO Double
analyseRawMessages texts word = getWordSentimentMap >>= \m ->
  let sentences = map toSentence texts
   in pure (analyseWordSentiments m word sentences)

-- | Type alias for `[Text]`. Assumed to be a list of words containing only
-- lowercase letters.
type Sentence = [Text]

-- | Turn a Text into a list of words containing only lowercase letters,
-- split by non-letter characters.
toSentence :: Text -> Sentence
-- TODO: make it split a text into actual sentences
toSentence = T.split (not . isLetter) . T.toLower

-- | Return the overall sentiment of sentences containing a particular word
analyseWordSentiments :: Map Text Bool -> Text -> [Sentence] -> Double
analyseWordSentiments sentimentMap word sentences =
  -- We do not want to include the target word itself in calculations, as it
  -- might be seen as positive or negative by itself
  let sentences' = map (filter (/=word)) . filter (word `elem`) $ sentences
   in analyseSentiments sentimentMap sentences'

-- | Return the overall positivity/negativity of a list of sentences
-- represented as a value between -1 and 1.
analyseSentiments :: Map Text Bool -> [Sentence] -> Double
analyseSentiments sentimentMap sentences =
  let sentiments = map (analyseSentiment sentimentMap) sentences
   in average sentiments

-- | Return the positivity/negativity of a single sentence 
-- as a value between -1 and 1.
analyseSentiment :: Map Text Bool -> Sentence -> Double
analyseSentiment sentimentMap sentence = case sentence of
  [] -> 0
  _  -> let bools = mapMaybe (`M.lookup` sentimentMap) sentence
            values = map (\b -> if b then 1 else -1) bools
         in average values

-- | Use the `positive-words.txt` and `negative-words.txt` files
-- to generate a dictionary of words mapped to Bools, where True
-- corresponds to positive and False corresponds to negative.
getWordSentimentMap :: IO (Map Text Bool)
getWordSentimentMap = do
  positiveWords <- drop 35 . T.lines <$> (T.readFile =<< getDataFileName "positive-words.txt")
  negativeWords <- drop 35 . T.lines <$> (T.readFile =<< getDataFileName "negative-words.txt")
  let pairs = map (,True) positiveWords ++ map (,False) negativeWords
  pure (M.fromList pairs)

average :: Fractional a => [a] -> a
average [] = 0
average a = sum a / (realToFrac . length) a

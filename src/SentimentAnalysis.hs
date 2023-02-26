module SentimentAnalysis ( analyseRawMessages ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Char (isLetter)

import SentimentAnalysis.SentimentData 
  ( SentimentData
  , getSentimentData
  , lookupSentiment )

-- | Given an unprocessed list of Texts and a target word, find the
-- average sentiment of the texts containing that word as a value
-- between -1 and 1.
analyseRawMessages :: [Text] -> Text -> IO Double
analyseRawMessages texts word = getSentimentData >>= \m ->
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
analyseWordSentiments :: SentimentData -> Text -> [Sentence] -> Double
analyseWordSentiments sentimentMap word sentences =
  -- We do not want to include the target word itself in calculations, as it
  -- might be seen as positive or negative by itself
  let sentences' = map (filter (/=word)) . filter (word `elem`) $ sentences
   in analyseSentiments sentimentMap sentences'

-- | Return the overall positivity/negativity of a list of sentences
-- represented as a value between -1 and 1.
analyseSentiments :: SentimentData -> [Sentence] -> Double
analyseSentiments sentimentMap sentences =
  let sentiments = map (analyseSentiment sentimentMap) sentences
   in average sentiments

-- | Return the positivity/negativity of a single sentence 
-- as a value between -1 and 1.
analyseSentiment :: SentimentData -> Sentence -> Double
analyseSentiment sentimentMap sentence = case sentence of
  [] -> 0
  _  -> let values = map (`lookupSentiment` sentimentMap) sentence
         in average values

average :: Fractional a => [a] -> a
average [] = 0
average a = sum a / (realToFrac . length) a

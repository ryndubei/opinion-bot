{-# LANGUAGE OverloadedStrings #-}
module SentimentAnalysis ( analyseRawMessages, wordInvariant, analyseRawMessage ) where

import Data.Text (Text)
import qualified Data.Text as T

import Data.Char (isPunctuation)

import NLP.Tokenize.Text
    ( punctuation, run, whitespace, Tokenizer, uris, EitherList (..) )

import SentimentAnalysis.SentimentData 
  ( SentimentData
  , lookupSentiment )
import Control.Monad ((>=>))
import Data.Either (isRight)

-- | Given an unprocessed list of Texts and a target word, find the
-- average sentiment of the texts containing that word as a double.
analyseRawMessages :: SentimentData -> [Text] -> Text -> Double
analyseRawMessages sentimentData texts word =
  let sentences = map toSentence texts
   in analyseWordSentiments sentimentData (head (toSentence word)) sentences

-- | Given a single unprocessed Text, get its sentiment as a double.
analyseRawMessage :: SentimentData -> Text -> Double
analyseRawMessage sentimentData text =
  let sentence = toSentence text
   in analyseSentiment sentimentData sentence

-- | Returns Right () when an input word is fine, otherwise returns
-- Left with an explanation.
wordInvariant :: Text -> Either Text ()
wordInvariant word
  | (length . toSentence) word /= 1 = Left "Keyword must not contain internal whitespace"
  | otherwise = Right ()

-- | Type alias for `[Text]`. Assumed to be a list of words containing only
-- lowercase letters.
type Sentence = [Text]

sentenceTokenizer :: Tokenizer
sentenceTokenizer text = (E . filter isRight . unE) $ 
  (whitespace >=> uris >=> punctuation >=> freezePunctuation) text
  where
    freezePunctuation :: Tokenizer
    freezePunctuation x
      | T.all isPunctuation x = E [Left x]
      | otherwise = E [Right x]

-- | Turn a Text into a list of words containing only lowercase letters,
-- split by non-letter characters.
toSentence :: Text -> Sentence
toSentence = run sentenceTokenizer . T.toLower

-- | Return the overall sentiment of sentences containing a particular word
analyseWordSentiments :: SentimentData -> Text -> [Sentence] -> Double
analyseWordSentiments sentimentMap word sentences =
  -- We do not want to include the target word itself in calculations, as it
  -- might be seen as positive or negative by itself
  let sentences' = 
        filter (not . null)
        . map (filter (/=word)) 
        . filter (word `elem`) 
        $ sentences
   in analyseSentiments sentimentMap sentences'

-- | Return the average sentiment of a list of sentences
analyseSentiments :: SentimentData -> [Sentence] -> Double
analyseSentiments sentimentMap sentences =
  let sentiments = map (analyseSentiment sentimentMap) sentences
   in average sentiments

-- | Return the sentiment of a single sentence 
analyseSentiment :: SentimentData -> Sentence -> Double
analyseSentiment sentimentMap sentence =
  let values = map (`lookupSentiment` sentimentMap) sentence
   in sum values

average :: (Foldable t, Fractional a) => t a -> a
average xs 
  | null xs = 0
  | otherwise = sum xs / (realToFrac . length) xs

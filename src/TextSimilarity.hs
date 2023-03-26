module TextSimilarity (findReply) where

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Text as T
import Data.List (intersect, maximumBy)
import Data.Ord (comparing)
import Data.Bifunctor (first)
import NLP.Tokenize.Text (tokenize)

-- | Given a list of texts and another text, find the text in the list that is
-- most likely to immediately follow the given text, together with confidence.
findReply :: [Text] -> Text -> Maybe (Text, Double)
findReply [] _ = Nothing
findReply [_] _ = Nothing
findReply texts text =
  let texts' = init . zip [1..] $ map (tokenize . T.toLower) texts
      text' = tokenize text
      cosSimilarities = map (\(i, t) -> (i, cosineSimilarity (makeVector text') (makeVector t))) texts'
      mostSimilar = maximumBy (comparing snd) cosSimilarities
   in Just (first (texts !!) mostSimilar)

type Vector = Map Text Double

makeVector :: [Text] -> Vector
makeVector = foldr (\t m -> M.insertWith (+) t 1 m) M.empty

dotProduct :: Vector -> Vector -> Double
dotProduct v1 v2 = sum [ (v1 M.! k) * (v2 M.! k) | k <- M.keys v1 `intersect` M.keys v2 ]

magnitude :: Vector -> Double
magnitude v = sqrt (sum [ x * x | x <- M.elems v ])

cosineSimilarity :: Vector -> Vector -> Double
cosineSimilarity v1 v2 = dotProduct v1 v2 / (magnitude v1 * magnitude v2)
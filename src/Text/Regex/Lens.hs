{-# LANGUAGE Rank2Types #-}

module Text.Regex.Lens
       ( regex
       , matched
       ) where

import Control.Lens
import Control.Applicative
import Text.Regex.Base
import Text.Regex.Posix
import qualified Data.Array as A

type RegexResult = [RegexPartialResult]

data RegexPartialResult
    = RegexMatchedPart String MatchArray
    | RegexOtherPart String
    deriving Show

regex :: String -> Lens' String RegexResult
regex pat f target = fromRegexResult <$> f (toRegexResult pat target)

matched :: Traversal' [RegexPartialResult] String
matched afb s = go s
  where
    go ((RegexMatchedPart str marr):xs) = (:) <$> (RegexMatchedPart <$> afb str <*> pure marr) <*> go xs
    go (x:xs) = (:) <$> pure x <*> go xs
    go [] = pure []

toRegexResult :: String -> String -> RegexResult
toRegexResult pat target = go 0 matchList
  where
    matchList = target =~ pat

    go pos [] = [RegexOtherPart (after pos target)]
    go pos (m:ms) =
        if posDiff > 0
            then RegexOtherPart (extract (pos, posDiff) target) : cont
            else cont
      where
        pl@(pos', len) = m A.! 0
        posDiff = pos' - pos
        cont = RegexMatchedPart (extract pl target) m : go (pos' + len) ms

fromRegexResult :: RegexResult -> String
fromRegexResult = concat . map toStr
  where
    toStr (RegexMatchedPart s _) = s
    toStr (RegexOtherPart s) = s

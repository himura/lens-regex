{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}

module Text.Regex.Lens
       ( regex
       , regex'
       , matched
       , matched'
       ) where

import Control.Lens
import Control.Applicative
import Text.Regex.Base
import Text.Regex.Posix
import qualified Data.Array as A

type RegexResult = [RegexPartialResult]

data RegexPartialResult
    = RegexMatchedPart (String, MatchArray)
    | RegexOtherPart String
    deriving Show

regex :: (Indexable Int p, Applicative f)
      => String
      -> p (String, MatchArray) (f (String, MatchArray))
      -> String -> f String
regex pat = regex' pat . matched

regex' :: String -> Lens' String RegexResult
regex' pat f target = fromRegexResult <$> f (toRegexResult pat target)

matched :: (Choice p, Indexable Int p, Applicative f, Traversable t)
        => p (String, MatchArray) (f (String, MatchArray))
        -> t RegexPartialResult -> f (t RegexPartialResult)
matched = traversed . matched'

matched' :: (Choice p, Applicative f) => Optic' p f RegexPartialResult (String, MatchArray)
matched' = dimap matchedStr toRes . right'
  where
    matchedStr (RegexMatchedPart s) = Right s
    matchedStr (RegexOtherPart s) = Left s

    toRes (Left s) = pure (RegexOtherPart s)
    toRes (Right x) = RegexMatchedPart <$> x

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
        cont = RegexMatchedPart ((extract pl target), m) : go (pos' + len) ms

fromRegexResult :: RegexResult -> String
fromRegexResult = concat . map toStr
  where
    toStr (RegexMatchedPart (s, _)) = s
    toStr (RegexOtherPart s) = s

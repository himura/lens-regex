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
    = RegexMatchedPart (String, [String])
    | RegexOtherPart String
    deriving Show

regex :: (Indexable Int p, Applicative f)
      => String
      -> p (String, [String]) (f (String, [String]))
      -> String -> f String
regex pat = regex' pat . matched

regex' :: String -> Lens' String RegexResult
regex' pat f target = fromRegexResult <$> f (toRegexResult pat target)

matched :: (Choice p, Indexable Int p, Applicative f, Traversable t)
        => p (String, [String]) (f (String, [String]))
        -> t RegexPartialResult -> f (t RegexPartialResult)
matched = traversed . matched'

matched' :: (Choice p, Applicative f) => Optic' p f RegexPartialResult (String, [String])
matched' = dimap matchedStr toRes . right'
  where
    matchedStr (RegexMatchedPart s) = Right s
    matchedStr (RegexOtherPart s) = Left s

    toRes (Left s) = pure (RegexOtherPart s)
    toRes (Right x) = RegexMatchedPart <$> x

toRegexResult :: String -> String -> RegexResult
toRegexResult pat target = go 0 matchList
  where
    matchList :: [A.Array Int (MatchOffset, MatchLength)]
    matchList = target =~ pat

    go pos [] = [RegexOtherPart (after pos target)]
    go pos (m:ms) =
        if posDiff > 0
            then RegexOtherPart (extract (pos, posDiff) target) : cont
            else cont
      where
        (pos', len) = m A.! 0
        posDiff = pos' - pos
        (matchStr:submatches) = map (flip extract target) $ A.elems m
        cont = RegexMatchedPart (matchStr, submatches) : go (pos' + len) ms

fromRegexResult :: RegexResult -> String
fromRegexResult = concat . map toStr
  where
    toStr (RegexMatchedPart (s, _)) = s
    toStr (RegexOtherPart s) = s

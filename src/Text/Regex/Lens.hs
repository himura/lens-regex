{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Lens
       ( MatchPart(..)
       , matchedString
       , backreferences
       , iregexTraverse
       , regexTraverse

       , regex
       , regex'
       , filterMatched
       ) where

import Control.Lens
import Control.Applicative
import Text.Regex.Base
import Text.Regex.Posix
import qualified Data.Array as A

type RegexResult = [RegexPartialResult]
type RegexPartialResult = Either String MatchPart

data MatchPart = MatchPart
    { _matchedString :: String
    , _backreferences :: [String]
    } deriving Show
makeLenses ''MatchPart

regex :: (Indexable Int p, Applicative f)
      => String
      -> p MatchPart (f MatchPart)
      -> String -> f String
regex pat = regex' pat . traversed . filterMatched

regex' :: String -> Lens' String RegexResult
regex' pat f target = fromRegexResult <$> f (toRegexResult pat target)

regexTraverse :: String -> Traversal' String MatchPart
regexTraverse pat f target = fromRegexResult <$> go (toRegexResult pat target)
  where
    go [] = pure []
    go ((Left x):xs) = ((Left x):) <$> go xs
    go ((Right x):xs) = (:) <$> (Right <$> f x) <*> go xs

iregexTraverse :: (Indexable Int p, Applicative f)
               => String
               -> p MatchPart (f MatchPart) -> String -> f String
iregexTraverse pat = conjoined (regexTraverse pat) (indexing (regexTraverse pat))

filterMatched :: (Choice p, Applicative f) => Optic' p f RegexPartialResult MatchPart
filterMatched = dimap id (either (pure . Left) (Right <$>)) . right'

toRegexResult :: String -> String -> RegexResult
toRegexResult pat target = go 0 matchList
  where
    matchList :: [A.Array Int (MatchOffset, MatchLength)]
    matchList = target =~ pat

    go pos [] = [Left (after pos target)]
    go pos (m:ms) =
        if posDiff > 0
            then Left (extract (pos, posDiff) target) : cont
            else cont
      where
        (pos', len) = m A.! 0
        posDiff = pos' - pos
        (ms0:mss) = map (flip extract target) $ A.elems m
        cont = Right (MatchPart ms0 mss) : go (pos' + len) ms

fromRegexResult :: RegexResult -> String
fromRegexResult = concat . map toStr
  where
    toStr (Right (MatchPart s _)) = s
    toStr (Left s) = s

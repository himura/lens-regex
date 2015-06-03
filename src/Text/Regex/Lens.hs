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

import Control.Applicative
import Control.Lens
import qualified Data.Array as A
import Data.Monoid
import Text.Regex.Base

type RegexResult text = [RegexPartialResult text]
type RegexPartialResult text = Either text (MatchPart text)

data MatchPart text = MatchPart
    { _matchedString :: text
    , _backreferences :: [text]
    } deriving Show
makeLenses ''MatchPart

regex :: (Indexable Int p, Applicative f, RegexLike regex text, Monoid text)
      => regex
      -> p (MatchPart text) (f (MatchPart text))
      -> text -> f text
regex pat = regex' pat . traversed . filterMatched

regex' :: (RegexLike regex text, Monoid text) => regex -> Lens' text (RegexResult text)
regex' pat f target = fromRegexResult <$> f (toRegexResult pat target)

regexTraverse :: (RegexLike regex text, Monoid text) => regex -> Traversal' text (MatchPart text)
regexTraverse pat f target = fromRegexResult <$> go (toRegexResult pat target)
  where
    go [] = pure []
    go ((Left x):xs) = ((Left x):) <$> go xs
    go ((Right x):xs) = (:) <$> (Right <$> f x) <*> go xs

iregexTraverse :: (Indexable Int p, Applicative f, RegexLike regex text, Monoid text)
               => regex
               -> p (MatchPart text) (f (MatchPart text)) -> text -> f text
iregexTraverse pat = conjoined (regexTraverse pat) (indexing (regexTraverse pat))

filterMatched :: (Choice p, Applicative f) => Optic' p f (RegexPartialResult text) (MatchPart text)
filterMatched = dimap id (either (pure . Left) (Right <$>)) . right'

toRegexResult :: RegexLike regex text => regex -> text -> (RegexResult text)
toRegexResult pat target = go 0 $ matchAll pat target
  where
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

fromRegexResult :: Monoid text => (RegexResult text) -> text
fromRegexResult = mconcat . map toStr
  where
    toStr (Right (MatchPart s _)) = s
    toStr (Left s) = s

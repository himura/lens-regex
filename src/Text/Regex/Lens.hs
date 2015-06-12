{-# LANGUAGE CPP #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Lens
       ( MatchPart(..)
       , matchedString
       , captures

       , regex
       , regex'
       , matched
       , matched'
       ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Monoid
#endif

import Control.Lens
import qualified Data.Array as A
import Text.Regex.Base

-- $setup
-- >>> import Text.Regex.Quote
-- >>> import Text.Regex.Posix
-- >>> :set -XQuasiQuotes

type RegexResult text = [RegexPartialResult text]
type RegexPartialResult text = Either text (MatchPart text)

data MatchPart text = MatchPart
    { _matchedString :: text
    , _captures :: [text]
    } deriving Show
makeLensesFor [("_matchedString", "matchedString")] ''MatchPart
makeLensesWith (lensRulesFor [("_captures", "captures")] & generateUpdateableOptics .~ False) ''MatchPart

-- | An indexed Traversal for matched part with regexp.
--
-- >>> "foo bar baz" ^? regex [r|b.*r|]
-- Just (MatchPart {_matchedString = "bar", _captures = []})
--
-- >>> "foo bar baz" ^? regex [r|hoge|]
-- Nothing
--
-- You can access to the matched string by using `matchedString`:
--
-- >>> "foo bar baz" ^? regex [r|b.*r|] . matchedString
-- Just "bar"
--
-- Multiple result:
--
-- >>> "foo bar baz" ^.. regex [r|b[^ ]+|] . matchedString
-- ["bar","baz"]
--
-- Replace:
--
-- >>> "foo bar baz" & regex [r|b[^ ]+|] . matchedString .~ "nya"
-- "foo nya nya"
--
-- Indexing:
--
-- >>> "foo bar baz" ^.. regex [r|b[^ ]+|] . index 1 . matchedString
-- ["baz"]
--
-- >>> "foo bar baz" & regex [r|b[^ ]+|] . index 1 . matchedString .~ "nya"
-- "foo bar nya"
--
-- Captures:
--
-- >>> "foo00 bar01 baz02" ^.. regex [r|([a-z]+)([0-9]+)|] . captures
-- [["foo","00"],["bar","01"],["baz","02"]]
--
-- >>> "foo00 bar01 baz02" ^.. regex [r|([a-z]+)([0-9]+)|] . captures . traversed . index 1
-- ["00","01","02"]
--
-- /Note/: This is /not/ a legal Traversal, unless you are very careful not to invalidate the predicate on the target.
-- For example, if you replace the matched part with a string which is not match with the regex, the second 'Traversal' law is violated.
--
-- @
-- let l = regex [r|t.*t|] . matchedString
-- 'Control.Lens.Setter.over' l (++ "peta") '.' 'Control.Lens.Setter.over' l (++ "nya") '/=' 'Control.Lens.Setter.over' l ((++ "peta") . (++ "nya"))
-- 'Control.Lens.Setter.over' l (++ "put") '.' 'Control.Lens.Setter.over' l (++ "hot") '==' 'Control.Lens.Setter.over' l ((++ "put") . (++ "hot"))
-- @
regex :: (RegexLike regex text, Monoid text)
      => regex -- ^ compiled regular expression
      -> IndexedTraversal' Int text (MatchPart text)
regex pat = regex' pat . matched

regex' :: (RegexLike regex text, Monoid text) => regex -> Lens' text (RegexResult text)
regex' pat f target = fromRegexResult <$> f (toRegexResult pat target)

matched :: (Indexable Int p, Applicative f)
        => p (MatchPart text) (f (MatchPart text)) -> RegexResult text -> f (RegexResult text)
matched = conjoined matched' (indexing matched')

matched' :: Traversal' (RegexResult text) (MatchPart text)
matched' f target = go target
  where
    go [] = pure []
    go ((Left x):xs) = ((Left x):) <$> go xs
    go ((Right x):xs) = (:) <$> (Right <$> f x) <*> go xs

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

{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Quote
       ( r
       )
       where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Regex.Base

-- | Generate compiled regular expression.
--
-- This QuasiQuote is shorthand of /makeRegex with type annotations/:
--
-- @
-- [r|hogehoge|] == (makeRegex ("hogehoge" :: String) :: Regex)
-- @
--
-- The /Regex/ type signature in the above example, is the type
-- which is named as /Regex/ in this translation unit.
-- Therefore, you can choose Regex type by changing imports.
--
-- For example, the /exp/ variable in the below example has the type of Text.Regex.Posix.Regex:
--
-- @
-- import Text.Regex.Posix (Regex)
-- exp = [r|hoge|]
-- @
--
-- and, the /exp/ variable in below example has the type of Text.Regex.PCRE.Regex:
--
-- @
-- import Text.Regex.PCRE (Regex)
-- exp = [r|hoge|]
-- @
r :: QuasiQuoter
r = QuasiQuoter
    { quoteExp = \str -> do
           mk <- [|makeRegex|]
           return $ mk `AppE` (LitE (StringL str) `SigE` ConT ''String) `SigE` ConT (mkName "Regex")
    , quotePat = error "quotePat is not defined"
    , quoteType = error "quoteType is not defined"
    , quoteDec = error "quoteDec is not defined"
    }

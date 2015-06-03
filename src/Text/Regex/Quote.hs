{-# LANGUAGE TemplateHaskell #-}

module Text.Regex.Quote
       ( r
       )
       where

import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Text.Regex.Base

r :: QuasiQuoter
r = QuasiQuoter
    { quoteExp = \str -> do
           mk <- [|makeRegex|]
           return $ mk `AppE` (LitE (StringL str) `SigE` ConT ''String)
    , quotePat = error "quotePat is not defined"
    , quoteType = error "quoteType is not defined"
    , quoteDec = error "quoteDec is not defined"
    }

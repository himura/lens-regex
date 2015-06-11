module Main where

import Test.DocTest
import System.Directory
import System.FilePath
import Data.List
import Control.Monad
import Control.Applicative

main :: IO ()
main = do
    sources <- findSources
    doctest $
        [ "-isrc"
        , "-idist/build/autogen"
        , "-optP-include"
        , "-optPdist/build/autogen/cabal_macros.h"
        ]
        ++ sources

findSources :: IO [FilePath]
findSources = filter (isSuffixOf ".hs") <$> go "src"
  where
    go dir = do
      (dirs, files) <- listFiles dir
      (files ++) . concat <$> mapM go dirs
    listFiles dir = do
      entries <- map (dir </>) . filter (`notElem` [".", ".."]) <$> getDirectoryContents dir
      (,) <$> filterM doesDirectoryExist entries <*> filterM doesFileExist entries

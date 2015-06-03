{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Lens
import Text.Regex.Lens
import Text.Regex.Posix
import Text.Regex.Quote

main :: IO ()
main = do
    let target1 = "hoge00 fuga hoge01 neko hoge02"
    putStrLn "##################"
    putStrLn "## Target String:"
    putStrLn $ "target1 = " ++ show target1

    putStrLn "\n## Example: target1 ^.. regex [r|hoge[0-9]+|]"
    print $ target1 ^.. regex ([r|hoge[0-9]+|] :: Regex)

    putStrLn "\n## Example: target1 ^.. regex [r|hoge[0-9]+|] . matchedString"
    print $ target1 ^.. regex ([r|hoge[0-9]+|] :: Regex) . matchedString

    putStrLn "\n## Example: target1 ^? regex [r|hoge[0-9]+|] . index 1 . matchedString"
    print $ target1 ^? regex ([r|hoge[0-9]+|] :: Regex) . index 1 . matchedString

    putStrLn "\n## Example: target1 ^? regex [r|hoge[0-9]+|] . index 3 . matchedString"
    print $ target1 ^? regex ([r|hoge[0-9]+|] :: Regex) . index 3 . matchedString

    putStrLn "\n## Example: target1 & regex [r|hoge[0-9]+|] . matchedString .~ \"HOGE\""
    print $ target1 & regex ([r|hoge[0-9]+|] :: Regex) . matchedString .~ "HOGE"

    putStrLn "\n## Example: target1 & regex [r|hoge[0-9]+|] .index 1 . matchedString .~ \"HOGE\""
    print $ target1 & regex ([r|hoge[0-9]+|] :: Regex) . index 1 . matchedString .~ "HOGE"

    putStrLn "\n## Example: target1 & regex [r|hoge[0-9]+|] .index 1 . matchedString %~ (\\s -> \"<<\" ++ s ++ \">>\")"
    print $ target1 & regex ([r|hoge[0-9]+|] :: Regex) . index 1 . matchedString %~ (\s -> "<<" ++ s ++ ">>")

    let target2 = "<img src=\"/image/shinku0721.jpg\" alt=\"shinku birthday\"><img src=\"/image/shinku141.jpg\">"
    putStrLn "##################"
    putStrLn "## Target String:"
    putStrLn $ "target2 = " ++ show target2

    putStrLn "\n## Example: target2 ^.. regex [r|<img src=\"([^\"]+)\"[^>]*>|] . captures . traversed . index 0"
    print $ target2 ^.. regex ([r|<img src="([^"]+)"[^>]*>|] :: Regex) . captures . traversed . index 0



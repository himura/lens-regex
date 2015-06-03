{-# LANGUAGE QuasiQuotes #-}

{-

OUTPUT:

##################
## Target String:
target1 = "hoge00 fuga hoge01 neko hoge02"

## Example: target1 ^.. regex [r|hoge[0-9]+|]
[MatchPart {_matchedString = "hoge00", _captures = []},MatchPart {_matchedString = "hoge01", _captures = []},MatchPart {_matchedString = "hoge02", _captures = []}]

## Example: target1 ^.. regex [r|hoge[0-9]+|] . matchedString
["hoge00","hoge01","hoge02"]

## Example: target1 ^? regex [r|hoge[0-9]+|] . index 1 . matchedString
Just "hoge01"

## Example: target1 ^? regex [r|hoge[0-9]+|] . index 3 . matchedString
Nothing

## Example: target1 & regex [r|hoge[0-9]+|] . matchedString .~ "HOGE"
"HOGE fuga HOGE neko HOGE"

## Example: target1 & regex [r|hoge[0-9]+|] .index 1 . matchedString .~ "HOGE"
"hoge00 fuga HOGE neko hoge02"

## Example: target1 & regex [r|hoge[0-9]+|] .index 1 . matchedString %~ (\s -> "<<" ++ s ++ ">>")
"hoge00 fuga <<hoge01>> neko hoge02"
##################
## Target String:
target2 = "<img src=\"/image/shinku0721.jpg\" alt=\"shinku birthday\"><img src=\"/image/shinku141.jpg\">"

## Example: target2 ^.. regex [r|<img src="([^"]+)"[^>]*>|] . captures . traversed . index 0
["/image/shinku0721.jpg","/image/shinku141.jpg"]

-}

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

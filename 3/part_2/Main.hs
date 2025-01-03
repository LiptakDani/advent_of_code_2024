module Main where

import Part2

main :: IO ()
main = do
    content <- readFile "input.txt"
    let modifiedContent = filterDoParts content
    let mulStrings = getMuls modifiedContent
    let result = sum $ map getLineMul mulStrings
    print result

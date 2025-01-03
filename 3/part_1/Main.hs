module Main where

import Part1

main :: IO ()
main = do
    content <- readFile "input.txt"
    let mulStrings = getMuls content
    let result = sum $ map getLineMul mulStrings
    print result

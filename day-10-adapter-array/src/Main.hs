module Main where

import Adapter (combinations, diffBetweenAdapters, parseAdapters)

main :: IO ()
main = do
    adapters <- parseAdapters "input.txt"
    putStrLn . ("part 1: " ++) . show . diffBetweenAdapters 3 $ adapters
    putStrLn . ("part 2: " ++) . show . combinations 3 $ adapters
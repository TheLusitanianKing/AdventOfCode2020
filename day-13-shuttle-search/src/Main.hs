module Main where

import Shuttle

main :: IO ()
main = do
    -- part 1
    parsed <- parseFile "input.txt"
    case parsed of
        Nothing        -> putStrLn "Could not parse the file"
        Just (ts, bus) ->
            putStrLn . ("part 1: " ++) . show . quickestDeparture ts $ bus
    -- part 2
    sequence <- parseFile' "test.txt"
    putStrLn . ("part 2: " ++) . show . sequenceT 0 $ sequence
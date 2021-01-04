module Main where

import Jigsaw (cornerTiles, parseFile)

main :: IO ()
main = do
    tm <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . product . cornerTiles $ tm
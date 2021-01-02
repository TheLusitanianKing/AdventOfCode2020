module Main where

import Jigsaw

main :: IO ()
main = do
    tm <- parseFile "test.txt"
    print . length $ tm
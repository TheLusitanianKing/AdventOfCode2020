module Main where

import Halting (instrcIndex, parseFile, readInstrctns)

main :: IO ()
main = do
    instructions <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . snd . readInstrctns $ instructions
    putStrLn . ("part 2: " ++) . show . instrcIndex $ instructions
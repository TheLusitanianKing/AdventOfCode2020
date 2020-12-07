module Main where

import Data.List (nub)
import Haversacks (nbChildren, parentColours, parseRulesFromFile)

main :: IO ()
main = do
    rules <- parseRulesFromFile "input.txt"
    putStrLn . ("part 1: " ++) . show . length $ parentColours rules "shiny gold"
    putStrLn . ("part 2: " ++) . show $ nbChildren rules "shiny gold"
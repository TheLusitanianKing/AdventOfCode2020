module Main where

import Prelude hiding (cycle)
import Cubes (cycle, parseFile)

main :: IO ()
main = do
    activeCubesCoordinates <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . length $ iterate cycle activeCubesCoordinates !! 6
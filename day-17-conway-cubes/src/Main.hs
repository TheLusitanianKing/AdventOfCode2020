module Main where

import Prelude hiding (cycle)
import Cubes (cycle, parseFile)

main :: IO ()
main = do
    activeCubesCoordinates <- parseFile 3 "input.txt"
    putStrLn . ("part 1: " ++) . show . length $ iterate cycle activeCubesCoordinates !! 6
    activeCubesCoordinates' <- parseFile 4 "input.txt"
    putStrLn . ("part 2: " ++) . show . length $ iterate cycle activeCubesCoordinates' !! 6
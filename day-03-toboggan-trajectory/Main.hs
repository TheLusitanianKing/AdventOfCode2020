module Main where

import Toboggan (parseFile, trajectory, encounteredTrees)

main :: IO ()
main = do
    map <- parseFile "input.txt"
    let start = (0, 0)
    putStrLn . ("part 1: " ++) . show . encounteredTrees $ trajectory map start 3 1
    let ts = [ encounteredTrees $ trajectory map start x y | (x, y) <- [(1,1), (3,1), (5,1), (7,1), (1,2)] ]
    putStrLn . ("part 2: " ++) . show . foldr1 (*) $ ts
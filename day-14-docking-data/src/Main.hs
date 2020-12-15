module Main where

import Docking (execute1, execute2, parseFile)

main :: IO ()
main = do
    is <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . execute1 $ is
    putStrLn . ("part 2: " ++) . show . execute2 $ is
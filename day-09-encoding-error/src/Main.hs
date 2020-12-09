module Main where

import Data.Maybe (fromJust)
import Encoding (findSum, parseFile, stop)

main :: IO ()
main = do
    ns <- parseFile "input.txt"
    let n = fromJust . stop 25 $ ns
    putStrLn . ("part 1: " ++) . show $ n
    let xs = fromJust . findSum ns $ n
    putStrLn . ("part 2: " ++) . show . sum $ [minimum xs, maximum xs]
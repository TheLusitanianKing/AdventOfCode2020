module Main where

import Monster

main :: IO ()
main = do
    (rs, s) <- parse "input.txt"
    let combs = combinations rs
    putStrLn . ("part 1: " ++) . show . length . filter (`elem` combs) $ s
module Main where

import Monster (combinations, matchSet', parse)

main :: IO ()
main = do
    (rs, s) <- parse "input.txt"
    let cs = combinations rs 0
    putStrLn . ("part 1: " ++) . show . length . filter (`elem` cs) $ s
    putStrLn . ("part 2: " ++) . show . length . filter (\s -> matchSet' s (combinations rs 42) (combinations rs 31)) $ s
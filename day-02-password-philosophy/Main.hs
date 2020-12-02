module Main where

import PasswordChecker (parseFile, check, check')

main :: IO ()
main = do
    passwords <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . length . filter (== True) . map check $ passwords
    putStrLn . ("part 2: " ++) . show . length . filter (== True) . map check' $ passwords
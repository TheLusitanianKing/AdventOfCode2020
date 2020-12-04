module Main where

import Passport (parse, parseFile, validPassport, validPassportData)

main :: IO ()
main = do
    passports <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . length . filter validPassportData $ passports
    putStrLn . ("part 2: " ++) . show . length . filter validPassport . map parse $ passports
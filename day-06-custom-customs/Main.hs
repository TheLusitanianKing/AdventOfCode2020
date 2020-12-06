module Main where

import qualified Data.Set as S
import Customs (answersFromGroup, answersFromGroup', parseFile)

main :: IO ()
main = do
    groups <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . sum . map (S.size . answersFromGroup) $ groups
    putStrLn . ("part 2: " ++) . show . sum . map (S.size . answersFromGroup') $ groups
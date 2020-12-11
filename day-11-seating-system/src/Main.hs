module Main where

import Seating (nbOccupiedSeats, nextRound1, nextRound2, parseFile, rounds)

main :: IO ()
main = do
    seats <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . nbOccupiedSeats . rounds nextRound1 $ seats
    putStrLn . ("part 2: " ++) . show . nbOccupiedSeats . rounds nextRound2 $ seats
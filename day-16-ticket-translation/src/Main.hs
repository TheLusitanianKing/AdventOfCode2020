module Main where

import Ticket (combinations, multiplyFieldsStartingWith, onlyPossibleCombination, parseFile, ticketScanningErrorRate)

main :: IO ()
main = do
    (cs, ticket, nearbyTickets) <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . ticketScanningErrorRate cs $ nearbyTickets
    putStrLn . ("part 2: " ++) . show
        . multiplyFieldsStartingWith ticket "departure " . onlyPossibleCombination . combinations cs $ nearbyTickets
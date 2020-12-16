module Main where

import Ticket (parseFile, ticketScanningErrorRate)

main :: IO ()
main = do
    (cs, ticket, nearbyTickets) <- parseFile "input.txt"
    putStrLn . ("part 1: " ++) . show . ticketScanningErrorRate cs $ nearbyTickets
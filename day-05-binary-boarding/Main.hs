module Main where

import Boarding (Plane(..), parse, parseFile, seatID)
import Data.List ((\\))

main :: IO ()
main = do
    seats <- parseFile "input.txt"
    let plane = Plane { rows = 128, columns = 8 }
    let maxSeatID = maximum . map (maybe 0 seatID . parse plane) $ seats
    putStrLn . ("part 1: " ++) . show $ maxSeatID
    putStrLn . ("part 2: " ++) . show . maximum . ([1..maxSeatID] \\) . map (maybe 0 seatID . parse plane)
        $ seats
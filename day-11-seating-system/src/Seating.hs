module Seating where

import Data.List ((\\))

data Seat  = Free | Occupied | Floor deriving (Eq, Show)
type Ferry = [[Seat]]

parseSeat :: Char -> Seat
parseSeat x = case x of
    'L' -> Free
    '#' -> Occupied
    _   -> Floor

printSeat :: Seat -> Char
printSeat x = case x of
    Free     -> 'L'
    Occupied -> '#'
    Floor    -> '.'

-- pretty print a ferry's seats
prettyPrintFerry :: Ferry -> IO ()
prettyPrintFerry []     = return ()
prettyPrintFerry (x:xs) = (print $ map printSeat x) >> prettyPrintFerry xs

-- get a ferry's seats from a file
parseFile :: FilePath -> IO Ferry
parseFile path = readFile path >>= (\i -> return (map (map parseSeat) . lines $ i))

-- get all adjacent coordinates of a coordinate
adjacentCoordinates :: (Int, Int) -> [(Int, Int)]
adjacentCoordinates (x, y) = [(a, b) | a <- [(x-1)..(x+1)], a >= 0, b <- [(y-1)..(y+1)], b >= 0] \\ [(x, y)]

-- get all adjacent (existing) seats of a seat coordinate
adjacentSeats :: (Int, Int) -> Ferry -> [Seat]
adjacentSeats c ss =
    map (\(x, y) -> ss !! y !! x) (filter (\(x, y) -> x < length (head ss) && y < length ss) (adjacentCoordinates c))

-- retrieve the number of adjacent occupied seats
nbOccupiedAdjacentSeat :: (Int, Int) -> Ferry -> Int
nbOccupiedAdjacentSeat c ss = length . filter (== Occupied) $ adjacentSeats c ss

-- from a ferry's state, goes to another state of a ferry
nextRound :: (Seat -> (Int, Int) -> Ferry -> Seat) -> Ferry -> Ferry
nextRound f ss = goNextRound (0, 0) [] [] 
    where goNextRound c@(x, y) mAcc lAcc
            | nextXOut && nextYOut = mAcc ++ [lAcc']
            | nextXOut             = goNextRound (0, y+1) (mAcc ++ [lAcc']) []
            | otherwise            = goNextRound (x+1, y) mAcc lAcc'
            where lAcc'    = lAcc ++ [seat']
                  nextXOut = x + 1 >= length (head ss)
                  nextYOut = y + 1 >= length ss
                  seat     = ss !! y !! x
                  seat'    = f seat c ss

-- from a ferry, gives the number of occupied seats in the entire ferry
nbOccupiedSeats :: Ferry -> Int
nbOccupiedSeats = sum . map (\x -> length . filter (== Occupied) $ x)

-- part 1 spec
nextRound1 :: Ferry -> Ferry
nextRound1 = nextRound f
    where f seat c ss = case seat of
                      Free     -> if nbOccupiedAdjacentSeat c ss == 0 then Occupied else Free
                      Occupied -> if nbOccupiedAdjacentSeat c ss >= 4 then Free else Occupied
                      _        -> seat

-- part 2 spec
nextRound2 :: Ferry -> Ferry
nextRound2 = nextRound f
    where f seat c ss = case seat of
                    Free     -> if nbVisibleOccupiedSeats c ss == 0 then Occupied else Free
                    Occupied -> if nbVisibleOccupiedSeats c ss >= 5 then Free else Occupied
                    _        -> seat

-- count number of visible occupied seat from a given coordinate
nbVisibleOccupiedSeats :: (Int, Int) -> Ferry -> Int
nbVisibleOccupiedSeats c ss =
    sum [
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x-1, y))   c ss, -- left
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x-1, y-1)) c ss, -- top-left diagonal
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x, y-1))   c ss, -- top
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x+1, y-1)) c ss, -- top-right diagonal
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x+1, y))   c ss, -- right
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x+1, y+1)) c ss, -- bottom-right diagonal
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x, y+1))   c ss, -- bottom
        fromEnum $ checkOccupiedSeat (\(x, y) -> (x-1, y+1)) c ss  -- bottom-left diagonal
    ]

-- check if there is a visible occupied seat in the given direction for a given coordinate
checkOccupiedSeat ::  ((Int, Int) -> (Int, Int)) -> (Int, Int) -> Ferry -> Bool
checkOccupiedSeat nextC c ss = doCount (nextC c)
    where doCount cx@(x, y)
            | xIsOut || yIsOut = False -- out of the ferry
            | seat == Occupied = True -- found an occupied seat
            | seat == Free     = False -- vision is blocked
            | otherwise        = doCount (nextC cx) -- vision isn't blocked, look up next
            where seat   = ss !! y !! x
                  xIsOut = x < 0 || x >= length (head ss)
                  yIsOut = y < 0 || y >= length ss

-- apply a function for a ferry rounds after rounds until the states of the ferry's seats are stable
rounds ::  (Ferry -> Ferry) -> Ferry -> Ferry
rounds f fr | nextF' == fr = fr
            | otherwise    = rounds f nextF'
              where nextF' = f fr
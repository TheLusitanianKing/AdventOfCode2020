module Boarding where

data Plane = Plane { rows :: Int, columns :: Int }
type Seat = (Int, Int)

parseFile :: FilePath -> IO [String]
parseFile path = readFile path >>= (\i -> return (lines i))

seatID :: Seat -> Int
seatID (x, y) = x * 8 + y

parse :: Plane -> String -> Maybe Seat
parse p s = doParse s (0, rows p - 1) (0, columns p - 1)
    where doParse [] (rl, ru) (cl, cu) | rl == ru && cl == cu = Just (rl, cl)
          doParse [] _ _ = Nothing -- could not determine seat, ignore the boarding pass
          doParse (x:xs) r@(rl, ru) c@(cl, cu) =
              case x of
                  'F' -> doParse xs (rl, floor $ middleRow) c
                  'B' -> doParse xs (ceiling $ middleRow, ru) c
                  'R' -> doParse xs r (ceiling $ middleColumn, cu)
                  'L' -> doParse xs r (cl, floor $ middleColumn)
                  _   -> Nothing -- wrong character, ignore the boarding pass
            where middleRow    = fromIntegral ru - fromIntegral (ru - rl) / 2
                  middleColumn = fromIntegral cu - fromIntegral (cu - cl) / 2
module Shuttle where

import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, mapMaybe)
import Text.Read (readMaybe)

-- | Parsing for part 1
parseFile :: FilePath -> IO (Maybe (Int, [Int]))
parseFile path = do
    content <- readFile path
    let (timestamp:bus) = lines content
    case (readMaybe :: String -> Maybe Int) timestamp of
        Nothing -> return Nothing
        Just ts -> return (Just (ts, parseBus $ head bus))

parseBus :: String -> [Int]
parseBus = mapMaybe ((readMaybe :: String -> Maybe Int) . filter (/= 'x')) . splitOn ","

-- | Part 2, we need the information about the 'X' this time
parseFile' :: FilePath -> IO [Maybe Integer]
parseFile' path = do
    content <- readFile path
    return (parseBus' $ head . tail . lines $ content)

parseBus' :: String -> [Maybe Integer]
parseBus' = map (readMaybe :: String -> Maybe Integer) . splitOn ","

-- | From a timestamp, a list of bus, get the bus w/ quickest departure multiplied by the time to wait
quickestDeparture :: Int -> [Int] -> Int
quickestDeparture t bs = busNumber * waitingTime
    where nextDepartures           = map (\b -> (b - (t `rem` b), b)) bs
            -- conveniently placed bus ID at the second place in the tuple to make the minimum function works how I want
          (busNumber, waitingTime) = minimum nextDepartures

-- | From a initial timestamp, a sequence, get the first timestamp that have this sequence
sequenceT :: Integer -> [Maybe Integer] -> Integer
sequenceT t seq = doSequence t seq (catMaybes seq)
    where doSequence t seq bs
            | haveFullSeq = t
            | otherwise   = doSequence t' seq bs
                where t' = if t `rem` incr == 0 then t+incr else t+1
                      (x, haveFullSeq) = haveSeq t bs seq
                      incr = product (take (fromIntegral x) bs)

-- | From a timestamp, a sequence, check if the sequence is respected at the given timestamp
haveSeq :: Integer -> [Integer] -> [Maybe Integer] -> (Integer, Bool)
haveSeq t bs seq = doHaveSeq 0 t bs seq
      where doHaveSeq n _ _ []      = (n, True)
            doHaveSeq n t bs (x:xs) = case x of
                Nothing -> if all (\b -> t `rem` b /= 0) bs then doHaveSeq n (t+1) bs xs else (n, False)
                Just bi  -> if t `rem` bi == 0              then doHaveSeq (n+1) (t+1) bs xs else (n, False)
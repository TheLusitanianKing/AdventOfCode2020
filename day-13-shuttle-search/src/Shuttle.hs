module Shuttle where

import Data.List (union)
import Data.List.Split (splitOn)
import Data.Maybe (catMaybes, fromJust, mapMaybe)
import Text.Read (readMaybe)

parseFile :: FilePath -> IO (Maybe (Int, [Int]))
parseFile path = do
    content <- readFile path
    let (timestamp:bus) = lines content
    case (readMaybe :: String -> Maybe Int) timestamp of
        Nothing -> return Nothing
        Just ts -> return (Just (ts, parseBus $ head bus))

parseBus :: String -> [Int]
parseBus = mapMaybe ((readMaybe :: String -> Maybe Int) . filter (/= 'x')) . splitOn ","

-- from a bus number, a timestamp T, get its first departure after T
departureAfterT :: Int -> Int -> Int
departureAfterT b t = head [ x | x <- [t..], x `rem` b == 0 ]

-- from a timestamp, a list of bus, get the bus w/ quickest departure multiplied by the time to wait
quickestDeparture :: Int -> [Int] -> Int
quickestDeparture t bs =
    let (bus, departure) = foldl1 fusion (map (\b -> (b, departureAfterT b t)) bs)
        fusion (b1, d1) (b2, d2)
            | d1 <= d2  = (b1, d1)
            | otherwise = (b2, d2)
    in bus * (departure - t)

-- part 2, we need the information about the 'X' this time
parseFile' :: FilePath -> IO [Maybe Integer]
parseFile' path = do
    content <- readFile path
    return (parseBus' $ head . tail . lines $ content)

parseBus' :: String -> [Maybe Integer]
parseBus' = map (readMaybe :: String -> Maybe Integer) . splitOn ","

-- from a initial timestamp, a sequence, get the first timestamp that have this sequence
-- TODO: adapt to Integer as t will be over Int limit for the full example
sequenceT :: Integer -> [Maybe Integer] -> Integer
sequenceT t seq = doSequence t seq (catMaybes seq)
    where doSequence t seq bs | haveFullSeq   = t
                              | otherwise     =
                                    if t `rem` incr == 0
                                    then doSequence (t+incr) seq bs
                                    else doSequence (t+1) seq bs
                                where (x, haveFullSeq) = haveSeq t bs seq
                                      incr = product (take (fromIntegral x) bs)

-- from a timestamp, a sequence, check if the sequence is respected at the given timestamp
haveSeq :: Integer -> [Integer] -> [Maybe Integer] -> (Integer, Bool)
haveSeq t bs seq = doHaveSeq 0 t bs seq
      where doHaveSeq n _ _ []      = (n, True)
            doHaveSeq n t bs (x:xs) = case x of
                Nothing -> if all (\b -> t `rem` b /= 0) bs then doHaveSeq n (t+1) bs xs else (n, False)
                Just bi  -> if t `rem` bi == 0              then doHaveSeq (n+1) (t+1) bs xs else (n, False)
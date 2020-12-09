module Encoding where

import Data.List ((\\))

parseFile :: FilePath -> IO [Integer]
parseFile path = readFile path >>= (\i -> return (map read . lines $ i))

-- from a list of numbers, check if two distinct of those adds up to a wanted number
twoDistinctSumsTo :: [Integer] -> Integer -> Bool
twoDistinctSumsTo l x = length [(a, b) | a <- l, b <- l, a /= b, a + b == x] > 0

-- from a preambule size, a list of numbers, it gives the first number that fails if there is one
stop :: Int -> [Integer] -> Maybe Integer
stop p l | length l <= p = error "List is smaller than the preambule length"
         | otherwise = doStop l p
            where doStop l@(_:xs) p
                    | length l <= p                  = Nothing
                    | twoDistinctSumsTo cuttedList n = doStop xs p
                    | otherwise                      = Just n
                    where cuttedList = take p l
                          n          = l !! p

-- from a list of integers, given a integer, return the list of contiguous set of numbers to sums to the given number
findSum :: [Integer] -> Integer -> Maybe [Integer]
findSum l n = doFindSum l' l' []
    where l' = l \\ [n]
          doFindSum [] _ _    = Nothing
          doFindSum l@(_:xs) (y:ys) buffer
            | sum buffer > n  = doFindSum xs xs []
            | sum buffer == n = Just buffer
            | otherwise       = doFindSum l ys (buffer ++ [y])
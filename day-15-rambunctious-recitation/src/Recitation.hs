module Recitation where

import qualified Data.Map.Strict as M

recitation :: [Int] -> Int -> Int
recitation is el
    | el <= length is = error "Cannot get initial number (yet)"
    | otherwise = doRecitate (last is) (length is + 1) initialMap
    where initialMap = M.fromList $ zipWith (\x y -> (x, [y])) is [1..]
          doRecitate x n spoken
            | n > el = x
            | M.member x spoken && length (spoken M.! x) >= 2 =
                doRecitate age (n + 1) (M.insertWith insertHelper age [n] spoken)
            | otherwise =
                doRecitate 0 (n + 1) (M.insertWith insertHelper 0 [n] spoken)
            where age = foldl1 (-) $ spoken M.! x
                  insertHelper new old | length old < 2 = new ++ old
                                       | otherwise      = new ++ [head old]
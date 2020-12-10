module Adapter where

import Data.List (sort)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

parseAdapters :: FilePath -> IO [Int]
parseAdapters path = readFile path >>= (\i -> return (map (read :: String -> Int). lines $ i))

-- from a maximum joltage and a list of adapters, returns the list of joltage differences between them
diffBetweenAdapters :: Int -> [Int] -> Int
diffBetweenAdapters m l =
    let doDiff _ [] acc     = m : acc
        doDiff n (x:xs) acc = doDiff x xs ((x - n) : acc)
        res = doDiff 0 (sort l) []
    in (length . filter (== 3) $ res) * (length . filter (== 1) $ res)

-- from a maximum joltage and a list of adapters, returns the possible combinations of adapters
combinations :: Int -> [Int] -> Int
combinations m l = doCombinations 0 initialPaths
    where initialPaths = Map.fromSet (\k -> if k == 0 then 1 else 0) (Set.fromList l')
          l'           = 0 : sort l
          doCombinations index paths
            | index >= length l' = maximum paths
            | otherwise          = doCombinations index' paths'
           where index'        = index + 1
                 adapter       = l' !! index
                 reachableAdps = filter (\x -> x > adapter && x - m <= adapter) l'
                 updatedPaths  = Map.unions $
                    map (\x -> Map.fromList [(x, (paths Map.! x) + (paths Map.! adapter))]) reachableAdps
                 paths'        = Map.union updatedPaths paths
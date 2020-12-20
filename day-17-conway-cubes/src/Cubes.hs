module Cubes where

import Data.List (nub)
import Data.Maybe (mapMaybe)

type Coordinate = [Int]

-- keeping active cubes coordinates only, do not keeping track of the inactive ones
parseFile :: Int -> FilePath -> IO [Coordinate]
parseFile dimension path = parse . lines <$> readFile path
    where parse = doParseMatrix 0 0 []
          doParseMatrix _ _ acc []     = acc
          doParseMatrix x y acc (c:cs) = doParseMatrix 0 (y+1) (doParseRow x y [] c ++ acc) cs
          doParseRow _ _ acc []     = acc
          doParseRow x y acc (c:cs) = doParseRow (x+1) y acc' cs
            where acc' = if c == '#' then ([x, y] ++ replicate (dimension - 2) 0):acc else acc

-- from a coordinate, gives all coordinates that can be considered neighbours
neighboursCoordinates :: Coordinate -> [Coordinate]
neighboursCoordinates (x:y:z:[]) =
    map (\(a, b, c) -> [a, b, c])
    [(x', y', z') | x' <- [(x-1)..(x+1)], y' <- [(y-1)..(y+1)], z' <- [(z-1)..(z+1)], (x,y,z) /= (x', y', z')]
neighboursCoordinates (x:y:z:w:[]) =
    map (\(a, b, c, d) -> [a, b, c, d])
    [(x', y', z', w') |
    x' <- [(x-1)..(x+1)], y' <- [(y-1)..(y+1)], z' <- [(z-1)..(z+1)], w' <- [(w-1)..(w+1)],
    (x,y,z,w) /= (x', y', z', w')]
-- TODO: find a better way to do this for all dimensions

-- from a coordinate and a list of coordinates where there is an active cube, gives active neighbour cubes
activeNeighbours :: Coordinate -> [Coordinate] -> [Coordinate]
activeNeighbours c = filter (\x -> x `elem` neighboursCoordinates c)

-- from a coordinate and a list of coordinates where there is an active cube, gives inactive neighbour cubes
inactiveNeighbours :: Coordinate -> [Coordinate] -> [Coordinate]
inactiveNeighbours c actives = filter (`notElem` actives) (neighboursCoordinates c)

-- from a list of coordinates, gives another after cycling once
cycle :: [Coordinate] -> [Coordinate]
cycle cs = mapMaybe inactiveF inactiveCoordinates ++ mapMaybe activeF cs
    where inactiveCoordinates = nub $ concatMap (`inactiveNeighbours` cs) cs
          inactiveF c = let nbActiveNeighbours = length (activeNeighbours c cs) in
              if nbActiveNeighbours == 3 then Just c else Nothing
          activeF   c = let nbActiveNeighbours = length (activeNeighbours c cs) in
              if nbActiveNeighbours == 2 || nbActiveNeighbours == 3 then Just c else Nothing
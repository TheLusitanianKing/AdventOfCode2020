module Cubes where

import Data.List (nub)
import Data.Maybe (mapMaybe)

type Coordinate = (Int, Int, Int)

-- keeping active cubes 3D coordinates only, do not keeping track of the inactive ones
parseFile :: FilePath -> IO [Coordinate]
parseFile path = parse . lines <$> readFile path
    where parse = doParseMatrix 0 0 []
          doParseMatrix _ _ acc []     = acc
          doParseMatrix x y acc (c:cs) = doParseMatrix 0 (y+1) (doParseRow x y [] c ++ acc) cs
          doParseRow _ _ acc []     = acc
          doParseRow x y acc (c:cs) = doParseRow (x+1) y acc' cs
            where acc' = if c == '#' then (x, y, 0):acc else acc

-- from a 3D coordinate, gives all 3D coordinates that can be considered neighbours
neighboursCoordinates :: Coordinate -> [Coordinate]
neighboursCoordinates (x, y, z) =
    [(x', y', z') | x' <- [(x-1)..(x+1)], y' <- [(y-1)..(y+1)], z' <- [(z-1)..(z+1)], (x,y,z) /= (x', y', z')]

-- from a 3D coordinate and a list of coordinates where there is an active cube, gives active neighbour cubes
activeNeighbours :: Coordinate -> [Coordinate] -> [Coordinate]
activeNeighbours c = filter (\x -> x `elem` neighboursCoordinates c)

-- from a 3D coordinate and a list of coordinates where there is an active cube, gives inactive neighbour cubes
inactiveNeighbours :: Coordinate -> [Coordinate] -> [Coordinate]
inactiveNeighbours c actives = filter (`notElem` actives) (neighboursCoordinates c)

-- from a list of 3D coordinates, gives another after cycling once
cycle :: [Coordinate] -> [Coordinate]
cycle cs = mapMaybe inactiveF inactiveCoordinates ++ mapMaybe activeF cs
    where inactiveCoordinates = nub $ concatMap (`inactiveNeighbours` cs) cs
          inactiveF c = let nbActiveNeighbours = length (activeNeighbours c cs) in
              if nbActiveNeighbours == 3 then Just c else Nothing
          activeF   c = let nbActiveNeighbours = length (activeNeighbours c cs) in
              if nbActiveNeighbours == 2 || nbActiveNeighbours == 3 then Just c else Nothing

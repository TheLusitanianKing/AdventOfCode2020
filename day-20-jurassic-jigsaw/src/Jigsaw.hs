module Jigsaw where

import Data.Char (isAlpha, isNumber, isSpace)
import Data.List (groupBy, sortBy, transpose)
import Data.List.Split (splitOn)

type Tile = [[Bool]]

-- | Parsing the input to a tilemap
parseFile :: FilePath -> IO [(Int, Tile)]
parseFile path = map parseTile . splitOn [[]] . lines <$> readFile path

-- | Parsing a tile from a list of string (no failure management)
parseTile :: [String] -> (Int, Tile)
parseTile (header:tile) = (tileId, map (map (=='#')) tile)
    where tileId = read . takeWhile isNumber . dropWhile isSpace . dropWhile isAlpha $ header

-- | Flip vertically
flipTile :: Tile -> Tile
flipTile = map reverse

-- | Rotating up 90 degrees
rotateTile :: Tile -> Tile
rotateTile = transpose . map reverse

-- | From an original tile, gives all the possible tiles by flipping and rotating it
tileValues :: Tile -> [Tile]
tileValues = apply [] functions
    where functions = id : replicate 3 rotateTile ++ [flipTile] ++ replicate 3 rotateTile
          apply acc []     _ = acc
          apply acc (f:fs) t = apply (t':acc) fs t' where t' = f t

-- | From a tile, get the 8 possible edge value (4 * 2)
edges :: Tile -> [Int]
edges t = map (edgeToInt . head) (tileValues t)
    where edgeToInt :: [Bool] -> Int
          edgeToInt = foldl (\a b -> a * 2 + (if b then 1 else 0)) 0

-- | From a list of tile (ID and content), get its 8 possible edges
-- Then, filter the uniques edges and finally get the corner tiles (return list of the corner tiles ID)
cornerTiles :: [(Int, Tile)] -> [Int]
cornerTiles = map (fst . head) -- we keep only the ID of the tile with unique edges
            . filter ((==4) . length) -- only corner edges will have 4 unique edges : 2 edges * 2 (flipped)
            . groupBy (\(a, _) (a', _) -> a == a') -- group by ID
            . sortBy (\(a, _) (a', _) -> compare a a') -- sort by ID
            . concat
            . filter ((==1) . length) -- get unique edges
            . groupBy (\(_, b) (_, b') -> b == b') -- group by edge
            . sortBy (\(_, b) (_, b') -> compare b b') -- sort by edge
            . concatMap (\(i, ts) -> map (\e -> (i, e)) $ edges ts) -- all edges of all tiles [(ID, [Edges..])]
module Jigsaw where

import Data.Char (isAlpha, isNumber, isSpace)
import Data.List (transpose)
import Data.List.Split (splitOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Array (Array)
import qualified Data.Array as A

type ID = Int
type Tile = [[Char]]
type TileMap = Map ID Tile          -- ^ map of all the available tiles
type Jigsaw = Array (Int, Int) (Int, Tile) -- ^ the jigsaw puzzle

-- | Parsing the input to a tilemap
parseFile :: FilePath -> IO TileMap
parseFile path = M.fromList . map parseTile . splitOn [[]] . lines <$> readFile path

-- | Parsing a tile from a list of string (no failure management)
parseTile :: [String] -> (Int, Tile)
parseTile (header:tile) = (tileId, tile)
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

-- | From a tile, get one of its borders
data Border = BorderRight | BorderTop | BorderLeft | BorderBottom
              deriving (Enum, Eq, Show)

border :: Border -> Tile -> [Char]
border BorderRight  = map last
border BorderTop    = head
border BorderLeft   = map head
border BorderBottom = last

-- | From a tile A and a tile B and a given border of the tile A
-- check if the tile B combines at the opposite border
combineBorder :: Border -> Tile -> Tile -> Bool
combineBorder b t1 t2 = border b t1 == border b' t2
    where b' = toEnum $ (fromEnum b - 2) `mod` 4 -- if left then right, if top then bottom and so on

-- | From a tile A and a tile B, check if they combine at one or multiple borders
combine :: Tile -> Tile -> [Border]
combine t1 t2 = filter (\b -> combineBorder b t1 t2) borders
    where borders = [BorderRight ..]

-- | Helper function to pretty print a tile
prettyPrintTile :: Tile -> IO ()
prettyPrintTile []     = return ()
prettyPrintTile (r:rs) = putStrLn r >> prettyPrintTile rs

-- | From a map of tiles, create a empty jigsaw
-- Only works with tile maps that have a perfect square number of tiles (1, 4, 9, 16 and so on..)
emptyJigsawFromTileMap :: TileMap -> Jigsaw
emptyJigsawFromTileMap tm = A.listArray ((0, 0), (n-1, n-1)) (repeat (0, []))
    where n = round $ sqrt (fromIntegral $ length tm)
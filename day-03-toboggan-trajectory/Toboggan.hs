module Toboggan (parseFile, trajectory, encounteredTrees) where

data Cell = Empty | Tree deriving (Eq, Show)
type Map = [[Cell]]
type Coordinate = (Int, Int)

parseCell :: Char -> Cell
parseCell '.' = Empty
parseCell '#' = Tree
parseCell _   = error "Wrong character"

parseFile :: FilePath -> IO Map
parseFile path = do
    input <- readFile path
    return (map (cycle . map parseCell) . lines $ input)

move :: Map -> Coordinate -> Int -> Int -> Maybe (Coordinate, Cell)
move m (x,y) right bottom =
    -- no need to check the other dimension as it is infinite
    if y' > maxY m then Nothing else Just ((x', y'), m !! y' !! x')
    where x' = x + right
          y' = y + bottom
          maxY m = (length m) - 1

trajectory :: Map -> Coordinate -> Int -> Int -> [Cell]
trajectory m c right bottom =
    case nextMove of
        Nothing -> []
        Just ((x', y'), cellValue) -> cellValue : trajectory m (x', y') right bottom
    where nextMove = move m c right bottom

encounteredTrees :: [Cell] -> Int
encounteredTrees = length . filter (== Tree)
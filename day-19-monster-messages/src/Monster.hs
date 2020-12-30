module Monster (
    combinations,
    matchSet',
    parse
) where

import Data.List.Split (splitOn)

type RuleID = Int
data Rule = Rule RuleID RuleData deriving (Eq, Show)
data RuleData = Value String
              | Union [RuleID] [RuleID]
              | Inter [RuleID]
                deriving (Eq, Show)

parse :: FilePath -> IO ([Rule], [String])
parse path = do
    ls <- lines <$> readFile path
    return (map parseRule (takeWhile (/= "") ls), tail . dropWhile (/= "") $ ls)

parseRule :: String -> Rule
parseRule s = Rule id dt
    where id = read $ takeWhile (/= ':') s
          dtRaw = tail $ dropWhile (/= ':') s
          dt = parseData dtRaw
          parseData d | '"' `elem` d = Value (takeWhile (/='"') . dropWhile (\x -> x==' ' || x=='"') $d)
                      | '|' `elem` d = Union (getNumbers $ takeWhile (/='|') d) (getNumbers . tail $ dropWhile (/='|') d)
                      | otherwise    = Inter (getNumbers d)
          getNumbers s = map (read :: String -> Int) . filter (not . null) . splitOn " " $ s

-- unsafe: it will fail if cannot found one (should not happen with AoC, therefore no need to handle failure)
getRuleNumber :: Int -> [Rule] -> RuleData
getRuleNumber n = head . map (\(Rule id d) -> d) . filter (\(Rule id _) -> id == n)

-- from a list of finite rules, get all the possible combinations (starting at rule n)
combinations :: [Rule] -> Int -> [String]
combinations rs n = getCombinations $ getRuleNumber n rs
    where getCombinations (Value s)     = [s]
          getCombinations (Union i1 i2) = getCombinations (Inter i1) ++ getCombinations (Inter i2)
          getCombinations (Inter xs)    = map concat . combine . map (\x -> getCombinations $ getRuleNumber x rs) $ xs
          -- from a list of list of a, returns all the possible combinations
          -- e.g. [["a"], ["ba", "ab"]] -> [["a","ba"],["a","ab"]]
          combine []     = [[]]
          combine (x:xs) = [ x':xs' | x' <- x, xs' <- combine xs]

-- handle special cases
-- from a string and a list of combinations, as long as we match the beginning of the string with a combination we remove it and go on
-- at the end we return the rest of the unmatched string, how many times it has matched and if it has matched at least the given number
matchSet :: String -> [String] -> Int -> (String, Bool, Int)
matchSet s cs atLeast = helper 0 s
    where l = length . head $ cs -- we know they always have the same size and the list can't be empty
          helper n ss | take l ss `elem` cs = helper (n+1) (drop l ss)
                      | otherwise          = (ss, n >= atLeast, n)

-- here we double the match, prefixes combinations then suffixes, should match the entire string
-- and have matched at least one more time prefixes than suffixes
matchSet' :: String -> [String] -> [String] -> Bool
matchSet' s prefix suffix = null rest' && b && b' && np > ns
    where (rest, b, np)   = matchSet s prefix 2
          (rest', b', ns) = matchSet rest suffix 1
module Ticket where

import Data.List ((\\), intersect, isPrefixOf, reverse, transpose)
import Data.List.Split (splitOn)
import Text.Regex.PCRE ((=~))

type Constraint = (String, [(Int, Int)])

-- from a file, gives :
--      the list of the constraints,
--      the list of your ticket information
--      the list of all nearby ticket information
parseFile :: FilePath -> IO ([Constraint], [Int], [[Int]])
parseFile path = do
    (cs:(_:ticket):(_:nearbyTickets):_) <- splitOn [[]] . lines <$> readFile path
    return (map parseContraint cs,
            map (read :: String -> Int) . concatMap (splitOn ",") $ ticket,
            map (map (read :: String -> Int) . splitOn ",") nearbyTickets)

parseContraint :: String -> Constraint
parseContraint s =
    case s =~ "([\\w ]+): (\\d+)-(\\d+) or (\\d+)-(\\d+)" :: (String, String, String, [String]) of
            (_, _, _, name:a:b:c:d:_) -> (name, [(read a, read b), (read c, read d)])
            _                         -> error $ "Could not parse the constraint: " ++ s

verifyConstraint :: Constraint -> Int -> Bool
verifyConstraint (_, xs) n = any (\(lower, upper) -> n >= lower && n <= upper) xs

-- from a list of constraint and a list of ticket information,
-- verify if all its number verify at least one constraint, if not returns it
verifyTicket :: [Constraint] -> [Int] -> [Int]
verifyTicket cs = doVerify []
    where doVerify acc [] = acc
          doVerify acc (n:ns)
            | any (`verifyConstraint` n) cs = doVerify acc ns
            | otherwise                     = doVerify (n:acc) ns

verifyTicket' :: [Constraint] -> [Int] -> Bool
verifyTicket' cs = all (\t -> any (`verifyConstraint` t) cs)

ticketScanningErrorRate :: [Constraint] -> [[Int]] -> Int
ticketScanningErrorRate cs tcks = sum (concatMap (verifyTicket cs) tcks)

-- from a ticket information and a list of constraints, gives the names of the verified ones
validedConstraints :: Int -> [Constraint] -> [String]
validedConstraints n = map fst . filter (`verifyConstraint` n)

-- from a list of constraints, a list of tickets, gives from each ticket information the constraints that it verifies
combinations :: [Constraint] -> [[Int]] -> [[String]]
combinations cs tickets = reverse $ doCombine [] css
    where validTickets = filter (verifyTicket' cs) tickets
          css = transpose $ map (map (`validedConstraints` cs)) validTickets
          doCombine acc []  = acc
          doCombine acc (i:is) = doCombine (foldl1 intersect i : acc) is

-- from a list of constraints solved, gives the only possible one (if there is one)
onlyPossibleCombination :: [[String]] -> [String]
onlyPossibleCombination = doCombination []
    where doCombination as l
            | all (\x -> length x == 1) l = concat l
            | otherwise                   =
                case filter (\x -> length x == 1 && head x `notElem` as) l of
                    []    -> error "Could not find a combination"
                    (u:_) -> doCombination (head u:as) (map (\x -> if x /= u then x \\ u else x) l)

multiplyFieldsStartingWith :: [Int] -> String -> [String] -> Int
multiplyFieldsStartingWith t startWith cmbs =
    product . map snd . filter (\(a, _) -> startWith `isPrefixOf` a) $ zip cmbs t
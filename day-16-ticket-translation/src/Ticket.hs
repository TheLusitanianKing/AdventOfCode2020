module Ticket where

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
verifyTicket cs ticketInfo = doVerify [] ticketInfo
    where doVerify acc [] = acc
          doVerify acc (n:ns)
            | any (flip verifyConstraint n) cs = doVerify acc ns
            | otherwise                        = doVerify (n:acc) ns

-- part 1
ticketScanningErrorRate :: [Constraint] -> [[Int]] -> Int
ticketScanningErrorRate cs tcks = sum (concatMap (verifyTicket cs) tcks)
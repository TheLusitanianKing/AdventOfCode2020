module Customs (
    answersFromGroup,
    answersFromGroup',
    parseFile
) where

import Data.List.Split (splitWhen)
import Data.Set (Set)
import qualified Data.Set as S

parseFile :: FilePath -> IO [[String]]
parseFile path = readFile path >>= (\i -> return (splitWhen null . lines $ i))

answersFromGroup :: [String] -> Set Char
answersFromGroup xs = getAnswers xs S.empty
    where getAnswers []     s = s
          getAnswers (y:ys) s = getAnswers ys (S.union currentAnswers s)
            where currentAnswers = S.fromList y

answersFromGroup' :: [String] -> Set Char
answersFromGroup' [] = S.empty
answersFromGroup' (x:xs) = getAnswers xs (S.fromList x)
    where getAnswers []     s = s
          getAnswers (y:ys) s = getAnswers ys (S.intersection s currentAnswers)
            where currentAnswers = S.fromList y
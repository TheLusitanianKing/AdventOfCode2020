module Haversacks where

import Data.List ((\\), nub)
import Data.List.Split (split, onSublist)
import Text.Regex.PCRE ((=~))

type Colour = String
data Rule = Rule {
    colour :: Colour,
    container :: [(Colour, Int)]
} deriving (Show)

parseRulesFromFile :: FilePath -> IO [Rule]
parseRulesFromFile path =
    readFile path >>= (\i -> return (map parseRule . lines $ i))

parseRule :: String -> Rule
parseRule s =
    case splitedLine of
        [colour, _, c] -> Rule colour (parseContains c)
        _              -> error $ "Could not parse this rule: " ++ s
    where splitedLine = split (onSublist " bags contain ") s

parseContains :: String -> [(Colour, Int)]
parseContains s = doParseContains s []
    where doParseContains s acc =
              case matched of
                  (_, _, rest, (n:c:_)) -> doParseContains rest ((c, read n :: Int):acc)
                  _                     -> acc
              where matched = s =~ "(\\d) ([\\w ]*) bag[s]?"
                                :: (String, String, String, [String])

parentColours :: [Rule] -> Colour -> [Colour]
parentColours rs c = nub $ doParentColours [] (directParents [c])
    where doParentColours acc []  = acc
          doParentColours acc ccs = doParentColours (acc ++ ccs) (directParents ccs)
          directParents csx = map colour directParentsRules
              where directParentsRules =
                        filter (\r -> length csx /= length (csx \\ map fst (container r))) rs

-- unsafe operation because of the head, considers the rule exists
ruleFromColour :: [Rule] -> Colour -> Rule
ruleFromColour rs c = head . filter (\r -> c == colour r) $ rs

nbChildren :: [Rule] -> Colour -> Int
nbChildren rs c = sum $ map (\c -> snd c + (snd c * nbChildren rs (fst c))) childContainers
    where childContainers = (container $ ruleFromColour rs c)
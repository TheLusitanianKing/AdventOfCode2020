module Halting where

import Data.List (break)
import Data.Maybe (mapMaybe, maybe)
import Text.Read (readMaybe)

data Instrctn a = Acc a | Jmp a | Nop a deriving (Show)

parseFile :: FilePath -> IO [Instrctn Int]
parseFile path = readFile path >>= (\i -> return (mapMaybe parseInstrctn . lines $ i))

parseInstrctn :: String -> Maybe (Instrctn Int)
parseInstrctn s =
    case breaked of
        (_, [])    -> Nothing -- could not find a breaking space
        (inst, nb) ->
            case inst of
                "acc" -> maybe Nothing (\n -> Just (Acc n)) mn
                "jmp" -> maybe Nothing (\n -> Just (Jmp n)) mn
                "nop" -> maybe Nothing (\n -> Just (Nop n)) mn
            where mn = readMaybe cleanNb :: Maybe Int
                  cleanNb = dropWhile (\x -> x == ' ' || x == '+') nb
    where breaked = break (==' ') s

-- first return value being an indicator of if the instructions terminate (meaning we get out of the whole pile)
-- and second value is the value of the accumulator (if it terminates or just before it starts rotating)
readInstrctns :: [Instrctn Int] -> (Bool, Int)
readInstrctns is = doReadInsts [] 0 0
    where doReadInsts visited acc index
            | index `elem` visited = (False, acc) -- has rotated, so it can not terminate
            | index >= length is   = (True, acc) -- value as it has terminated
            | otherwise            =
                case currentInst of
                    Acc n -> doReadInsts visited' (acc + n) (index + 1)
                    Jmp n -> doReadInsts visited' acc (index + n)
                    Nop _ -> doReadInsts visited' acc (index + 1)
                where currentInst = is !! index
                      visited'    = index:visited

-- brute-forcing switching every jmp to nop or nop to jmp until we find a sequence of instructions that terminates
instrcIndex :: [Instrctn Int] -> Int
instrcIndex is = doInstrcIndex 0
    where doInstrcIndex index
            | index + 1 >= length is = error "Could not find any instruction that makes it terminate"
            | fst readIs             = snd readIs -- find a terminating instruction pile, read it
            | otherwise              = doInstrcIndex index' -- go on to the next instruction that can be changed
                where index' = index + 1
                      newIns = switchInsts is index
                      readIs = readInstrctns newIns

-- switching one instruction from the pile
switchInsts :: [Instrctn Int] -> Int -> [Instrctn Int]
switchInsts is i = doSwitchInsts is [] 0
    where doSwitchInsts [] acc _ = acc
          doSwitchInsts (x:xs) acc index
            | i == index =
                case x of
                    Acc _ -> doSwitchInsts xs (acc ++ [x]) index'
                    Jmp n -> doSwitchInsts xs (acc ++ [Nop n]) index'
                    Nop n -> doSwitchInsts xs (acc ++ [Jmp n]) index'
            | otherwise  = doSwitchInsts xs (acc ++ [x]) index'
            where index' = index + 1
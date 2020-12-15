module Docking (execute1, execute2, parseFile) where

import Data.Char (digitToInt, intToDigit)
import qualified Data.Map.Strict as M
import Data.List (isPrefixOf)
import Numeric (showIntAtBase)
import Text.Regex.PCRE ((=~))

data Instruction = Mask String | MemValue (Integer, Integer) deriving (Show)

parseFile :: FilePath -> IO [Instruction]
parseFile path = map parseInstruction . lines <$> readFile path

parseInstruction :: String -> Instruction
parseInstruction s
    | "mask" `isPrefixOf` s =
        case s =~ "mask = (\\w+)" :: (String, String, String, [String]) of
            (_, _, _, n:_) -> Mask n
            _              -> error $ "Wrong mask: " ++ s
    | "mem" `isPrefixOf` s =
        case s =~ "mem\\[(\\d+)\\] = (\\d+)" :: (String, String, String, [String]) of
            (_, _, _, addr:val:_) -> MemValue (read addr, read val)
            _                     -> error $ "Wrong memory value: " ++ s
    | otherwise = error $ "Could not parse instruction: " ++ s

-- from a binary string to its integer value
fromBinaryToInt :: String -> Integer
fromBinaryToInt cs = foldl (\acc x -> acc * 2 + fromIntegral (digitToInt x)) 0 (dropWhile (=='0') cs)

-- from a binary mask and a value n, gives the value after applying the mask to it
maskedValue :: String -> Integer -> Integer
maskedValue mask n = fromBinaryToInt $ doMasking (reverse binN) (reverse mask) []
    where binN = showIntAtBase 2 intToDigit n ""
          doMasking []       []       acc = acc
          doMasking []       m        acc = reverse (map (\x -> if x == 'X' then '0' else x) m) ++ acc
          doMasking _        []       _   = error "Mask is smaller than the value.."
          doMasking v@(x:xs) m@(y:ys) acc = case y of
                'X' -> doMasking xs ys (x:acc)
                _   -> doMasking xs ys (y:acc)

-- from a mask, a target address, gives all the target addresses
targetedAddresses :: String -> Integer -> [Integer]
targetedAddresses mask addr = map (fromBinaryToInt) $ doMasking (reverse binAddr) (reverse mask) []
    where binAddr = showIntAtBase 2 intToDigit addr ""
          doMasking []       []       acc = [acc]
          doMasking _        []       _   = error "Mask is smaller than the value.."
          doMasking []       m@(y:ys) acc = case y of
                'X' -> doMasking [] ys ('0':acc) ++ doMasking [] ys ('1':acc)
                _   -> doMasking [] ys (y:acc)
          doMasking v@(x:xs) m@(y:ys) acc = case y of
                '1' -> doMasking xs ys (y:acc)
                'X' -> doMasking xs ys ('0':acc) ++ doMasking xs ys ('1':acc)
                _   -> doMasking xs ys (x:acc)

-- common pattern for executing instruction piles
execute :: (M.Map Integer Integer -> Integer -> Integer -> String -> M.Map Integer Integer) -> [Instruction] -> Integer
execute f is = foldl1 (+) $ doExecute is [] M.empty
    where doExecute []     _    m = m
          doExecute (x:xs) mask m = case x of
              Mask m'              -> doExecute xs m' m
              MemValue (addr, val) -> doExecute xs mask (f m addr val mask)

-- part 1 execution
execute1 :: [Instruction] -> Integer
execute1 = execute (\m addr val mask -> M.insert addr (maskedValue mask val) m)

-- part 2 execution
execute2 :: [Instruction] -> Integer
execute2 = execute f
    where f m addr val mask = M.union mapVal m
            where mapVal = M.unions $ map (\a -> M.insert a val M.empty) addrs
                  addrs  = targetedAddresses mask addr
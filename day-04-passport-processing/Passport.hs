module Passport (
    parse,
    parseFile,
    validPassport,
    validPassportData
) where

import Data.Maybe (maybe, isJust)
import Data.List ((\\), intercalate, sort)
import Data.List.Split (splitWhen)
import Text.Read (readMaybe)
import Text.Regex.PCRE ((=~))

type PassportData = [(String, String)]
data Passport = Passport { -- if one field have data, it has been have been validated
    byr :: Maybe Int, -- four digits; at least 1920 and at most 2002
    iyr :: Maybe Int, -- four digits; at least 2010 and at most 2020
    eyr :: Maybe Int, -- four digits; at least 2020 and at most 2030
    hgt :: Maybe String, -- a number followed by either cm or in
        -- If cm, the number must be at least 150 and at most 193
        -- If in, the number must be at least 59 and at most 76
    hcl :: Maybe String, -- a # followed by exactly six characters 0-9 or a-f
    ecl :: Maybe String, -- exactly one of: amb blu brn gry grn hzl oth
    pid :: Maybe String -- a nine-digit number, including leading zeroes
} deriving (Show)

parseFile :: FilePath -> IO [PassportData]
parseFile path = do
    input <- readFile path
    return (map (map (break (==':')) . splitWhen (==' ') . intercalate " ") . splitWhen null . lines $ input)

validPassportData :: PassportData -> Bool
validPassportData p =
    let passportFields = (map fst . filter (\x -> length (snd x) > 1) $ p) in
        ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"] \\ passportFields == []

parse :: PassportData -> Passport
parse d = doParse blankPassport d
    where doParse p [] = p
          doParse p ((field, (_:value)):xs) = doParse (transformP field value p) xs
          doParse p (_:xs) = doParse p xs
          blankPassport = (Passport Nothing Nothing Nothing Nothing Nothing Nothing Nothing)

transformP :: String -> String -> Passport -> Passport
transformP field value p = case field of
    "byr" -> maybe p f (readMaybe value :: Maybe Int)
             where f x | x >= 1920 && x <= 2002 = p { byr = Just x }
                   f x | otherwise              = p
    "iyr" -> maybe p f (readMaybe value :: Maybe Int)
             where f x | x >= 2010 && x <= 2020 = p { iyr = Just x }
                   f x | otherwise              = p
    "eyr" -> maybe p f (readMaybe value :: Maybe Int)
             where f x | x >= 2020 && x <= 2030 = p { eyr = Just x }
                   f x | otherwise              = p
    "hgt" ->
        let match = value =~ "(\\d+)(in|cm)" :: (String, String, String, [String]) in
        case match of 
            (_, _, r, n:"in":_) | null r ->
                maybe p f (readMaybe n :: Maybe Int)
                where f x | x >= 59 && x <= 76 = p { hgt = Just value }
                      f x | otherwise          = p
            (_, _, r, n:"cm":_) | null r ->
                maybe p f (readMaybe n :: Maybe Int)
                where f x | x >= 150 && x <= 193 = p { hgt = Just value }
                      f x | otherwise            = p
            _ -> p
    "hcl" ->
        let match = value =~ "(#[a-f0-9]{6})" :: (String, String, String, [String]) in
        case match of
            (_, _, r, s:_) | null r -> p { hcl = Just s }
            _                       -> p
    "ecl" | value `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"] ->
        p { ecl = Just value }
    "pid" ->
        let match = value =~ "(\\d{9})" :: (String, String, String, [String]) in
        case match of
            (_, _, r, s:_) | null r -> p { pid = Just s }
            _                       -> p
    _     -> p

validPassport :: Passport -> Bool
validPassport p =  isJust (byr p) && isJust (iyr p) && isJust (eyr p) && isJust (iyr p)
                && isJust (hgt p) && isJust (hcl p) && isJust (ecl p) && isJust (pid p)
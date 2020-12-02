module PasswordChecker (parseFile, check, check') where

import Control.Exception (handle, SomeException(..))
import Data.Maybe (maybe)
import Text.Regex.PCRE ((=~))
import System.IO (hClose, hGetContents, openFile, readFile, IOMode(ReadMode))

data Password = Password Char Int Int String
                deriving (Show)

parseFile :: FilePath -> IO [Maybe Password]
parseFile path = handle (\(SomeException _) -> return []) $ do
    input <- readFile path
    return (map parse . lines $ input)

parse :: String -> Maybe Password
parse s =
    case matched of
        (_, _, _, min:max:forcedLetter:password:_)
            -> Just (Password (head forcedLetter) (read min) (read max) password)
        _   -> Nothing
    where matched = s =~ "(\\d+)-(\\d+) (\\w): (\\w+)"
                    :: (String, String, String, [String])

check :: Maybe Password -> Bool
check p =
    maybe False doCheck p
    where doCheck (Password l x y pass) = appears >= x && appears <= y
            where appears = length . filter (== l) $ pass

check' :: Maybe Password -> Bool
check' p =
    maybe False doCheck p
    where doCheck (Password l x y pass) = appears == 1
            where appears = length . filter (== True) $ [pass !! (x-1) == l, pass !! (y-1) == l]
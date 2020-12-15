module Main where

import Recitation (recitation)

main :: IO ()
main = do
    let n = [2,0,6,12,1,3]
    putStrLn . ("part 1: " ++) . show $ recitation n 2020
    putStrLn . ("part 2: " ++) . show $ recitation n 30000000
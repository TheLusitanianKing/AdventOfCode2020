module Main where

import Navigation (execute, execute', executeAll, initialShip, manhattan, parseInstructions)

main :: IO ()
main = do
    is <- parseInstructions "input.txt"
    putStrLn . ("part 1: " ++) . show . manhattan . executeAll execute initialShip $ is
    putStrLn . ("part 2: " ++) . show . manhattan . executeAll execute' initialShip $ is
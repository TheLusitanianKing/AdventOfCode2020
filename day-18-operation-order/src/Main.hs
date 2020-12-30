module Main where

import Resolver (evaluateOp, parseFile, replaceOps)

main :: IO ()
main = do
    input <- parseFile "input.txt"
    values <- mapM evaluateOp input
    print . sum $ values
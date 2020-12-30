module Main where

import Resolver (evaluateOp, parseFile, replaceOps, replaceOps')

main :: IO ()
main = do
    input <- parseFile "input.txt"
    values <- mapM (evaluateOp . replaceOps) input
    putStrLn . ("part 1: " ++) . show . sum $ values
    values' <- mapM (evaluateOp . replaceOps') input
    putStrLn . ("part 2: " ++) . show . sum $ values'
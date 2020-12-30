module Resolver where

import Data.List.Utils (replace)
import qualified Language.Haskell.Interpreter as Hint
import Operation (multipl, plus)

parseFile :: FilePath -> IO [String]
parseFile path = lines <$> readFile path

-- part 1
replaceOps :: String -> String
replaceOps = replace "*" "`multipl`" . replace "+" "`plus`"

-- part 2
replaceOps' :: String -> String
replaceOps' = replace "*" "`multipl'`" . replace "+" "`plus'`"

-- evaluating a string to its value (crashes an error if it didn't succeed for any reason)
evaluateOp :: String -> IO Integer
evaluateOp s = do
    i <- Hint.runInterpreter $ do
        Hint.loadModules ["src/Operation.hs"]
        Hint.setImports ["Prelude", "Operation"]
        Hint.eval s
    case i of
        Left _ -> error $ "Could not evaluate this expression: " ++ s
        Right r -> return (read r)
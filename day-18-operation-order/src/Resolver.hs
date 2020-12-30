module Resolver where

import Data.List.Utils (replace)
import qualified Language.Haskell.Interpreter as Hint
import Operation (multipl, plus)

parseFile :: FilePath -> IO [String]
parseFile path = map replaceOps . lines <$> readFile path

replaceOps :: String -> String
replaceOps = replace "*" "`multipl`" . replace "+" "`plus`"

evaluateOp :: String -> IO Integer
evaluateOp s = do
    i <- Hint.runInterpreter $ do
        Hint.loadModules ["src/Operation.hs"]
        Hint.setImports ["Prelude", "Operation"]
        Hint.eval s
    case i of
        Left _ -> error "..."
        Right r -> return (read r)
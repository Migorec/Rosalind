module Rosalind.Problems.REVP where

import Rosalind.Acids


main = do str <- getLine
          mapM_ (\(p,l) -> putStrLn (show p ++ " " ++ show l)) $ reversePalindromes  (read str) 4 12

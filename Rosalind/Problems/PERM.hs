module Rosalind.Problems.PERM where

import Data.List (permutations)

main = do str <- getLine
          let perm = permutations [1 .. read str]
          print $ length perm
          mapM_ (\l -> do mapM_ (\v -> putStr (show v ++ " ")) l
                          putStrLn "") perm

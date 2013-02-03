module Rosalind.Problems.SIGN where

import Data.List (permutations)

sp 0 = [[]]
sp n = (map (1:) r) ++ (map (-1:) r)
    where r =sp (n-1)

main = do str <- getLine
          let n = read str
              ps = sp n
              res = concat $ map (\p -> map (zipWith (*) p) ps) $ permutations [1..n]
          print $ length res
          mapM_ (\l -> do mapM_ (\x -> putStr (show x ++ " ")) l
                          putStrLn ""
                ) res

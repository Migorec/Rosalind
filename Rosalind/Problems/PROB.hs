module Rosalind.Problems.PROB where

import Rosalind.Acids


main = do str <- getLine
          nums <- getLine
          let dna = read str
          mapM_ (\s -> putStr ((show $ dnaRandomProbLog dna $ read s)  ++ " ")) $ words nums
          putStrLn ""

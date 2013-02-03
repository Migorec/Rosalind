module Rosalind.Problems.PROT where

import Rosalind.Proteins
import Rosalind.Acids

main = do str <- getLine
          putStrLn $ translate $ read str

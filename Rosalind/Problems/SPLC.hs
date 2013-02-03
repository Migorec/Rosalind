module Rosalind.Problems.SPLC where

import Rosalind.Acids
import Rosalind.Proteins


main = do str <- getLine
          strs <- getContents
          let dna = foldl (\a s -> removeSublist s a) str $ lines strs
          putStrLn $ translate $ transcribe $ read dna
       

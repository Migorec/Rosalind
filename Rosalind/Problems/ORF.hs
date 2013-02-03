module Rosalind.Problems.ORF where

import Rosalind.Acids
import Rosalind.Proteins
import Data.List (nub)



main = do str <- getLine
          let dna = read str
          mapM_ putStrLn $ nub $ (translateAll $ transcribe $ reverseComplement dna) ++ (translateAll $ transcribe dna)

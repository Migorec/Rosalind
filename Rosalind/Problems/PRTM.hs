module Rosalind.Problems.PRTM where

import Rosalind.Proteins

main = do str <- getLine
          print $ sum $ map massTable str

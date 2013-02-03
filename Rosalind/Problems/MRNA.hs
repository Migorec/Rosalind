module Rosalind.Problems.MRNA where

import Rosalind.Proteins


main = do str <- getLine
          print $ mod (rnaVars str) 1000000

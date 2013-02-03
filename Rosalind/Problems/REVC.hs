module Rosalind.Problems.REVC where

import Rosalind.Acids

main = do str <- getLine
          (print.reverseComplement.read) str

module Rosalind.Problems.RNA where

import Rosalind.Acids

main = do str <- getLine
          print $ transcribe $ read str

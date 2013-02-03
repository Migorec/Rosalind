module Rosalind.Problems.HAMM where

import Rosalind.Acids

main = do str1 <- getLine
          str2 <- getLine
          print $ hammingDistance (read str1) (read str2)

module Rosalind.Problems.RSTR where

import Rosalind.Acids 



main = do str1 <- getLine
          str2 <- getLine
          let [n,gc] = map read $ words str1
          let p = 10 ** (dnaRandomProbLog (read str2) gc)
          print (1 -  (1-p)**n)

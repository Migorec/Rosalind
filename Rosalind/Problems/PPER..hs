module Rosalind.Problems.PPER where

import Math.Combinatorics.Exact.Binomial

main = do str <- getLine
          let [n,k] = map read $ words str
          print ((choose n k) * (product [1..k]) `mod` 1000000)

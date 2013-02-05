module Rosalind.Problems.ASPC where

import Math.Combinatorics.Exact.Binomial

main = do str <- getLine
          let [n,m] = map read $ words str
          print $ (sum $ map (choose n) [m..n]) `mod` 1000000

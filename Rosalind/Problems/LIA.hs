module Rosalind.Problems.LIA where

import Math.Combinatorics.Exact.Binomial

solve :: Int -> Int -> Double
solve k n = sum $ map (\x -> (fromIntegral $ choose pop x) * 0.25^^x * 0.75^^(pop-x) ) [n ..pop]
    where pop = 2 ^ k

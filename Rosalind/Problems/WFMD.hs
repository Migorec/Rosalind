module Rosalind.Problems.WFMD where

import Math.Combinatorics.Exact.Binomial


generation :: [Double] -> [Double]
generation l = foldl (zipWith (+)) (repeat 0) $
                map (\(i,p) -> let q = fromIntegral i / fromIntegral n 
                               in map (\k -> p*(fromIntegral $ choose n k) * q^^k * (1-q)^^(n-k)) [0..n]
                    ) $ zip [0, 1.. ] l
    where n = length l - 1

main = do str <- getLine
          let [n,m,g,k] = map read $ words str
              gs = foldl (.) id $ replicate g generation
          print $ sum $ drop k $ gs (replicate (2*n - m) 0 ++ [1] ++ replicate m 0)

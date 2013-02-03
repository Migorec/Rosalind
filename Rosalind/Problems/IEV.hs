module Rosalind.Problems.IEV where


probs = [1,1,1,0.75,0.5,0]

main = do str <- getLine
          print $ sum $ map (*2) $ zipWith (*) probs $ map read $ words str

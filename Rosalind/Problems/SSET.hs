module Rosalind.Problems.SSET where


main = do str <- getLine
	  print $ 2 ^ (read str) `mod` 1000000
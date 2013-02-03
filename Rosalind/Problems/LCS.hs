module Rosalind.Problems.LCS where

import Rosalind.Acids

main = do strs <- getContents
          print $ sharedMotif $ map read $ lines strs

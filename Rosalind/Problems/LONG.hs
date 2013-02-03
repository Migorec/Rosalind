module Rosalind.Problems.LONG where

import Rosalind.Acids

main = do strs <- getContents
          print $ reconstruct $ map read $ lines strs

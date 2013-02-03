module Rosalind.Problems.GRPH where

import Rosalind.FASTA


main = do path <- getLine
          fss <- readFASTA path
          let res = kAdjacencyList 3 fss
          mapM_ (\(fs,fss) -> mapM_ (\fs2 -> putStrLn ((label fs) ++ " " ++ (label fs2))) fss) res

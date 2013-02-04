module Rosalind.Problems.KMER where


import Rosalind.Acids

main = do strs <- getContents
          mapM_ (\i -> putStr (show i ++ " ")) $ kMerComposition 4 $ read $ concat $ tail $ lines strs 



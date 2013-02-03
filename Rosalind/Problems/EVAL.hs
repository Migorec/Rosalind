module Rosalind.Problems.EVAL where

import Rosalind.Acids 


main = do str1 <- getLine
          str2 <- getLine
          str3 <- getLine
          let dna = read str2
              m = fromIntegral $ (read str1) - (length str2) + 1
          mapM_ (\x -> putStr (show x ++ " ")) $ map (\gcs -> m * 10 ** dnaRandomProbLog dna (read gcs))  $ words str3
          putStrLn ""

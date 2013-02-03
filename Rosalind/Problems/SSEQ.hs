module Rosalind.Problems.SSEQ where

import Rosalind.Acids


main = do str1 <- getLine
          str2 <- getLine
          mapM_ (\x -> putStr (show x ++ " ")) $ splicedSubstring str2 str1

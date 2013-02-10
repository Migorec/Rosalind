module Rosalind.Problems.KMP where

import Rosalind.List
import Control.Applicative
import Rosalind.FASTA
import Rosalind.Acids

test s = (length s) == (length $ failureList s) 

main = do path <- getLine
          fss <- readFASTA path
          let Acid l = string $ head fss
          mapM_ (\x -> putStr (show x ++ " ")) $ failureList l


          --failureList <$> getLine >>= mapM_ (\x -> putStr (show x ++ " ")) 
--main = do s <- getLine
--          print $ (length s) == (length $ failureList s)

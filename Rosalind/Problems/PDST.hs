module Rosalind.Problems.PDST where

import Rosalind.Acids
import Rosalind.FASTA

main = do path <- getLine
          dnas <- readFASTA path
          mapM_ (\l -> do mapM_ (\x -> putStr (show x ++ " ")) l
                          putStrLn ""
                ) $ pDistanceMatrix $ map string dnas

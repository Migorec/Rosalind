module Rosalind.Problems.GC where


import Rosalind.Acids
import Rosalind.FASTA
import Data.List (maximumBy)

main = do path <- getLine 
          fss <- readFASTA path
          let gcs = map (\(FS l dna) -> (l, gcContent dna)) fss
              res = maximumBy (\(_,v1) (_,v2) -> compare v1 v2) gcs
          putStrLn $ fst res
          putStrLn $ (show $ snd res) ++ "%"
       

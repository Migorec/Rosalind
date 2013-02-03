module Rosalind.Problems.CONS where

import Rosalind.Acids
import Data.List (unzip5)

main = do path <- getLine 
          cont <- readFile path
          let dnas = map read $ lines cont
              prof = profile dnas
              (as,ts,gs,cs,_) = unzip5 prof
              cons = consensus prof
          print dnas
          print cons
          putStr "A:"
          mapM_ (\x -> putStr $ ' ':(show x)) as
          putStrLn ""
          putStr "C:"
          mapM_ (\x -> putStr $ ' ':(show x)) cs
          putStrLn ""
          putStr "G:"
          mapM_ (\x -> putStr $ ' ':(show x)) gs
          putStrLn ""
          putStr "T:"
          mapM_ (\x -> putStr $ ' ':(show x)) ts
          putStrLn ""

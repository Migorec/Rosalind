module Rosalind.Problems.SUBS where


import Rosalind.Acids
import Data.List (intersperse)

main = do str <- getLine
          pat <- getLine
          mapM_ putStr $ intersperse " " $ map show $ locations (read pat) (read str)

module Rosalind.Problems.AFRQ where

main = do str <- getLine
          mapM_ (\x -> putStr (show x ++ " ")) $ map (\x -> x + 2*(sqrt x - x)) $ map read $ words str

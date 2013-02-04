module Rosalind.Problems.SETO where

import Data.List

mshow x = "{" ++ ( intercalate ", " $ map show x) ++ "}"

main = do str <- getLine
          s1 <- getLine
          s2 <- getLine
          let u = [1.. read str]
              set1 = read ("[" ++ (tail.init) s1 ++ "]")
              set2 = read ("[" ++ (tail.init) s2 ++ "]")
          putStrLn ""
          putStrLn $ mshow $ set1 `union` set2
          putStrLn $ mshow $ set1 `intersect` set2
          putStrLn $ mshow $ set1 \\ set2
          putStrLn $ mshow $ set2 \\ set1
          putStrLn $ mshow $ u \\ set1
          putStrLn $ mshow $ u \\ set2

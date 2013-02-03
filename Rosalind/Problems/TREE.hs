module Rosalind.Problems.TREE where

import Data.List ((\\))

reacheable :: Int -> [Int] -> [(Int,Int)] -> [Int]
reacheable v vs g = v : (concat $ map (\x -> reacheable x vs' (g \\ r)) t)
    where r = filter (\(a,b) -> a==v || b==v) g
          t = map (\(a,b) -> if a==v then b else a) r
          vs' = vs \\ t

trees :: [Int] -> [(Int,Int)] -> [[Int]]
trees [] _ = []
trees (v:vs) g = t : trees (vs \\ t) g
    where t = reacheable v vs g
    

main = do str <- getLine
          strs <- getContents
          let n = read str
              g = map (\s -> let [a,b] = map read $ words s in (a,b)) $ lines strs
          print $ trees [1..n] g
          print $ (length $ trees [1..n] g) - 1

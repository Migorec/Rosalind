module Rosalind.Problems.NWCK where

import Data.Tree.Newick
import Data.Tree
import Data.List (intercalate, find)
import Data.Maybe (fromJust)


pairs (s1:s2:_:ss) = (s1,s2):(pairs ss)
pairs [s1,s2] = [(s1,s2)]
pairs [] = []

shorter :: (Eq a) => ([a], [a]) -> ([a],[a])
shorter ([],x) = ([],x)
shorter (x,[]) = (x,[])
shorter (x:xs,y:ys) | x==y = shorter (xs,ys)
                    | otherwise = (x:xs,y:ys)

path :: Tree String -> String -> Maybe [Int]
path tree cat = fst $ p [] 1 tree
    where 
          p l i (Node y []) | y ==cat = (Just (l ++ [i]), i+1)
                            | otherwise = (Nothing, i+1)
          p l i (Node y sts)| y ==cat = (Just (l ++ [i]), i+1)
                            |otherwise = let nl = l ++ [i]
                                             (fi,fr) = foldl (\(i,tl) st -> let (r,ri) = p (nl) (i) st
                                                                            in (ri,tl ++ [r])
                                                             ) (i+1,[]) sts
                                         in case find (/=Nothing) fr of
                                                Nothing -> (Nothing, fi)
                                                Just x -> (x, fi)

solve :: (String,String) -> Int
solve (ts,cd) = length p1 + length p2
    where [cat, dog] = words cd 
          tree = readNewick ts
          catP = fromJust $ path tree cat
          dogP = fromJust $ path tree dog
          (p1,p2) = shorter (catP, dogP)

main = do strs <- getContents
          putStrLn $ intercalate " " $ map (show.solve) $ pairs $ lines strs 
          

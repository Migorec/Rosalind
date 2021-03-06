module Rosalind.List where

import Data.List (isPrefixOf, groupBy, sort)


kMers :: Int -> [a] -> [[a]]
kMers 0 _ = [[]]
kMers n alph = concat $ map (\a -> map (a:) $ kMers (n-1) alph) alph 

removeSublist :: (Eq a) => [a] -> [a] -> [a]
removeSublist pat l = f l
    where len = length pat
          f [] = []
          f l | pat `isPrefixOf` l = f $ drop len l
              | otherwise = (head l) : (f $ tail l)

                                                               
sublistsLoc :: (Eq a) => [a] -> [a] -> [Int]
sublistsLoc pat l | null pat = [1 .. length l + 1]
                  | otherwise = search 1 l
    where search n s | null s = []
                     | pat `isPrefixOf` s = n : search (n+1) (tail s)
                     | otherwise = search (n+1) (tail s)
                     
                     
sublists :: [a] -> [[a]]
sublists l = foldl (\a b -> a ++ (f b len l)) [] [len, len-1 .. 1]
    where len = length l
          f n len l | n > len = []
                    | otherwise = (take n l) : (f n (len - 1) (tail l)) 


splicedSubstring :: (Eq a) => [a] -> [a] -> [Int]
splicedSubstring  p s= f [] 1 p s
    where f a _ [] _ = a
          f _ _ _ [] = []
          f a n (p:ps) (s:ss) | p==s = f (a++[n]) (n+1) ps ss
                              | otherwise = f (a) (n+1) (p:ps) ss
                              

data Trie k l = Leaf k | Node k [(l,Trie k l)] deriving Show


trie :: (Enum k, Ord l) => [[l]] -> k -> Trie k l
trie lss k = snd $ trie' (sort lss) k 
    where trie' [] k = (succ k, Leaf k)
          trie' lls k = (fst res, Node k $ snd res)
            where res = foldl (\(k,r) lss -> let (kn, t) = trie' (filter (/=[]) $ map tail lss) k
                                             in (kn, (head $ head lss,t):r)
                              ) 
                              (succ k, []) $ groupBy (\(l1:ls1) (l2:ls2) -> l1==l2) lls
            

sharedInit :: (Eq a) => [a] -> [a] -> [a]
sharedInit [] _ = []
sharedInit _ [] = []
sharedInit (a:as) (b:bs) | a==b = a:(sharedInit as bs)
                         | otherwise = []


failureList list = 0 : (f (tail list) (replicate (length $ tail list) 0))
    where f [] [] = []
          f l fl = case sharedInit l list of
                    [] -> (head fl) : (f (tail l) (tail fl))
                    si -> let n = length si 
                              (fl':fls) = (zipWith max [1..n] $ take n fl) ++ (drop n fl)
                          in fl':(f (tail l) fls)





module Rosalind.Problems.REAR where

import Data.List (find, elemIndex)
import Data.Maybe (fromJust)

applyReverse (a,b) s = (take (a-1) s) ++ (reverse $ drop (a-1) $ take b s) ++(drop b s)

reverses :: (Eq a) => [a] -> [a] -> [(Int, Int)]
reverses as bs = reverse $ f [as] [([],as)]
    where f ss s = let r = find (\(_,x) -> x == bs) s
                       sss = filter (\(_,x) -> not $ elem x ss) $ concat $ map (\(r,s) -> map (\x -> (x:r, applyReverse x s) ) rs) s
                   in case r of
                       Just (x,_) -> x
                       Nothing -> f (ss ++ map snd sss) sss 
          n = length as
          rs = [(a,b) | a <- [1..n-1], b <- [2..n], a<b]
          
          
pairs (a:b:as) = (a,b):(pairs as)
pairs _ = []


fff as bs = map (\b -> fromJust $ elemIndex b as) bs

coutReverses :: (Eq a) => [a] -> [a] -> Int
coutReverses as bs = f1 $ map (\b -> fromJust $ elemIndex b as) bs
    where f1 [a] = 0
          f1 (a:b:as) | a<b = f1 (b:as)
                      | otherwise = 1 + f2 (b:as)
          f2 [a] = 0
          f2 (a:b:as) | a>b = f2 (b:as)
                      | otherwise = 1 + f1 (b:as)
          
main = do strs <- getContents
          let dat = pairs $ map (\s -> map read $ words s :: [Int]) $ filter (/="") $ lines strs
          mapM_ (\x -> putStr (show x ++ " ")) $ map (\(a,b) -> coutReverses a b) dat

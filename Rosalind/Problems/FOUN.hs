module Rosalind.Problems.FOUN where

import Math.Combinatorics.Exact.Binomial
import Data.List (transpose)

generation :: [Double] -> [Double]
generation l = foldl (zipWith (+)) (repeat 0) $
                map (\(i,p) -> let q = fromIntegral i / fromIntegral n 
                               in map (\k -> p*(fromIntegral $ choose n k) * q^^k * (1-q)^^(n-k)) [0..n]
                    ) $ zip [0, 1.. ] l
    where n = length l - 1
    
    
main = do str1 <- getLine
          str2 <- getLine
          let [n,m] = map read $ words str1
              a = map read $ words str2
              res =map (\k -> map (logBase 10 . head) $ 
                              take m $ 
                              tail $ 
                              iterate generation (replicate k 0 ++ [1] ++ replicate (2*n - k) 0)
                       ) a
          mapM_ (\x -> do mapM_ (\y -> putStr (show y ++ " ")) x
                          putStrLn "") $ transpose res         

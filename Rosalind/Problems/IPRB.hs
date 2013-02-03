module Rosalind.Problems.IPRB where

solve :: (Fractional a) => a -> a -> a -> a
solve k m n =1 - (n/len*(n-1)/(len-1) + n/len*m/(len-1) + m/len*(m-1)/(len-1)/4)
    where len = k + m + n

main = do str <- getLine
          let [k,m,n] = map read $ words str
          print $ solve k m n
       

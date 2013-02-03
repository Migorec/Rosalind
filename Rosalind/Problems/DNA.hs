module Rosalind.Problems.DNA where

import Rosalind.Acids

main = do str <- getLine
          let (a,t,g,c,_) = countBase $ read str 
          putStrLn (show a ++ " " ++ show c ++ " " ++ show g ++ " " ++ show t)

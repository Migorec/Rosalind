module Rosalind.Problems.MEND where

import Data.Tree.Newick
import Data.Tree

solve :: Tree String -> (Double,Double,Double)
solve (Node "AA" []) = (1,0,0)
solve (Node "Aa" []) = (0,1,0)
solve (Node "aa" []) = (0,0,1)
solve (Node _ [t1,t2]) = let (x1,y1,z1) = solve t1
                             (x2,y2,z2) = solve t2
                         in (x1*x2 + x1*y2/2 + y1*x2/2 + y1*y2/4,
                             x1*y2/2 + x1*z2 + y1*x2/2 + y1*y2/2 + y1*z2/2 + z1*x2 + z1*y2/2,
                             y1*y2/4 + y1*z2/2 + z1*y2/2 + z1*z2)

main = do str <- getLine
          let (x,y,z) = solve $ readNewick str
          putStrLn (show x ++ " " ++ show y ++ " " ++ show z)

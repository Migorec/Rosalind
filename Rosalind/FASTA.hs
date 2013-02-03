module Rosalind.FASTA (FASTAlabel,
                       FASTAstring (..),
                       readFASTA,
                       kAdjacencyList
                      ) where

import Rosalind.Acids

type FASTAlabel = String

data FASTAstring = FS {label :: FASTAlabel,
                       string :: DNA
                      }
                      

separate :: (Eq a) => a -> [a] -> [[a]]
separate k s | r==[] = [s]
             | otherwise = l:(separate k $ tail r) 
    where (l,r) = break (==k) s

readFASTA :: FilePath -> IO [FASTAstring]
readFASTA path = do str <- readFile path
                    return $ map (\ s -> let (l:str) = lines s
                                         in FS l (read $ concat str)) $ tail $ separate '>' str
                                         
kAdjacencyList :: Int -> [FASTAstring] -> [(FASTAstring,[FASTAstring])]
kAdjacencyList k l = map (\fss -> (fss, filter (\fss' -> isKAdjacent k (string fss) (string fss')) l)) l 







                    

module Rosalind.Proteins (translate, translateAll, rnaVars, massTable, identify) where

import Rosalind.Acids
import Data.Map (fromList, lookup)
import Prelude hiding (lookup)
import Control.Monad (liftM)
import Data.Tuple (swap)
import Data.Monoid
import Data.List (find)


table = fromList [("UUU", 'F'), ("CUU", 'L'), ("AUU", 'I'), ("GUU", 'V'),
                  ("UUC", 'F'), ("CUC", 'L'), ("AUC", 'I'), ("GUC", 'V'),
                  ("UUA", 'L'), ("CUA", 'L'), ("AUA", 'I'), ("GUA", 'V'),
                  ("UUG", 'L'), ("CUG", 'L'), ("AUG", 'M'), ("GUG", 'V'),
                  ("UCU", 'S'), ("CCU", 'P'), ("ACU", 'T'), ("GCU", 'A'),
                  ("UCC", 'S'), ("CCC", 'P'), ("ACC", 'T'), ("GCC", 'A'),
                  ("UCA", 'S'), ("CCA", 'P'), ("ACA", 'T'), ("GCA", 'A'),
                  ("UCG", 'S'), ("CCG", 'P'), ("ACG", 'T'), ("GCG", 'A'),
                  ("UAU", 'Y'), ("CAU", 'H'), ("AAU", 'N'), ("GAU", 'D'),
                  ("UAC", 'Y'), ("CAC", 'H'), ("AAC", 'N'), ("GAC", 'D'),
                  ("CAA", 'Q'), ("AAA", 'K'), ("GAA", 'E'),
                  ("CAG", 'Q'), ("AAG", 'K'), ("GAG", 'E'),
                  ("UGU", 'C'), ("CGU", 'R'), ("AGU", 'S'), ("GGU", 'G'),
                  ("UGC", 'C'), ("CGC", 'R'), ("AGC", 'S'), ("GGC", 'G'),
                  ("CGA", 'R'), ("AGA", 'R'), ("GGA", 'G'),
                  ("UGG", 'W'), ("CGG", 'R'), ("AGG", 'R'), ("GGG", 'G')] 

vars 'F' = 2
vars 'L' = 6
vars 'I' = 3
vars 'V' = 4
vars 'M' = 1
vars 'S' = 6
vars 'P' = 4
vars 'T' = 4
vars 'A' = 4
vars 'Y' = 2
vars 'H' = 2
vars 'N' = 2
vars 'D' = 2
vars 'Q' = 2
vars 'K' = 2
vars 'E' = 2
vars 'C' = 2
vars 'R' = 6
vars 'G' = 4
vars 'W' = 1 


mTable = [('A', 71.03711),
          ('C', 103.00919),
          ('D', 115.02694),
          ('E', 129.04259),
          ('F', 147.06841),
          ('G', 57.02146),
          ('H', 137.05891),
          ('I', 113.08406),
          ('K', 128.09496),
          ('L', 113.08406),
          ('M', 131.04049),
          ('N', 114.04293),
          ('P', 97.05276),
          ('Q', 128.05858),
          ('R', 156.10111),
          ('S', 87.03203),
          ('T', 101.04768),
          ('V', 99.06841),
          ('W', 186.07931),
          ('Y', 163.06333)]

mMap = fromList mTable

-- ????
massTable c = maybe 0 id $ lookup c mMap

reverseMass :: Double -> Maybe Char
reverseMass m = fmap fst $ find (\(_,v) -> abs (v-m) < 0.001) mTable


identify :: [Double] -> Maybe String
identify l =mconcat $ map (fmap (:[]) . reverseMass) $ zipWith (-) (tail l) l

rnaVars :: String -> Integer
rnaVars s = 3 * (product $ map vars s)

translate :: RNA -> String
translate rna = maybe "" id $ translate' rna
    
    
translate' :: RNA -> Maybe String
translate' (Acid l) = t' l
    where t' (a:b:c:bs) = maybe (Just []) (\ch -> liftM (ch:) $ t' bs) $ lookup (show $ Acid [a,b,c]) table
          t' _ = Nothing
    
    
translateAll :: RNA -> [String]
translateAll rna = filter (/="") $ f rna
    where f rna@(Acid (Adenin:Uracil:Guanine:ss)) = (translate rna):(translateAll $ Acid ss)
          f (Acid (s:ss)) = f $ Acid ss
          f (Acid [])  = []



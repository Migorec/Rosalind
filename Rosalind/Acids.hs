module Rosalind.Acids  where

import Data.List (isPrefixOf, maximumBy, inits, minimumBy, isInfixOf, find)
import Data.Maybe (fromJust)
import Rosalind.List 
import Text.Regex.TDFA

data Base = Adenin | Cytosine | Guanine | Thymine | Uracil deriving Eq

instance Show Base where 
    show Adenin = "A"
    show Cytosine = "C"
    show Guanine = "G"
    show Thymine = "T"
    show Uracil = "U"
    
    
instance Read Base where
    readsPrec _ "A" = [(Adenin, "")]
    readsPrec _ "C" = [(Cytosine, "")]
    readsPrec _ "G" = [(Guanine, "")]
    readsPrec _ "T" = [(Thymine, "")]
    readsPrec _ "U" = [(Uracil, "")]

newtype Acid = Acid [Base] deriving Eq

instance Show Acid where
    show (Acid l) = map (head.show) l
    
instance Read Acid where
    readsPrec _ s = [(Acid $ map (\char -> read [char] ) s,"")]

addBase :: (Num a) => (a,a,a,a,a) -> Base -> (a,a,a,a,a)
addBase (a, t, g, c, u) Adenin = (a+1, t, g, c, u)
addBase (a, t, g, c, u) Thymine = (a, t+1, g, c, u)
addBase (a, t, g, c, u) Guanine = (a, t, g+1, c, u)
addBase (a, t, g, c, u) Cytosine = (a, t, g, c+1, u)
addBase (a, t, g, c, u) Uracil = (a, t, g, c, u+1)

countBase :: (Num a) => Acid -> (a,a,a,a,a)
countBase (Acid l) = foldl addBase (0,0,0,0,0) l
     

type DNA = Acid
type RNA = Acid

transcribe :: DNA -> RNA
transcribe (Acid l) = Acid $ map (\base -> if base==Thymine 
                                           then Uracil
                                           else base) l
                             
class Complement a where
    complement :: a -> a                             
              
instance Complement Base where
    complement Adenin = Thymine
    complement Thymine = Adenin
    complement Cytosine = Guanine
    complement Guanine = Cytosine
    
instance Complement Acid where
    complement (Acid l) = Acid $ map complement l
    
reverseComplement :: DNA -> DNA
reverseComplement dna = Acid $ reverse comp
    where Acid comp = complement dna
    
gcContent :: (Fractional a) => DNA -> a
gcContent dna@(Acid l) = (g+c) * 100 / len
    where (_,_,g,c,_) = countBase dna
          len = fromIntegral $ length l
          
hammingDistance :: (Num a) => DNA -> DNA -> a
hammingDistance (Acid l1) (Acid l2) = sum $ zipWith (\b1 b2 -> if b1==b2
                                                               then 0
                                                               else 1) l1 l2
  
  
pDistance :: (Fractional a) => DNA -> DNA -> a
pDistance dna1@(Acid l) dna2 = (hammingDistance dna1 dna2) / (fromIntegral $ length l)
 
  
pDistanceMatrix :: (Fractional a) => [DNA] -> [[a]]
pDistanceMatrix dnas = map (\dna -> map (pDistance dna) dnas) dnas
  
                     

locations :: DNA -> DNA -> [Int]
locations (Acid l1) (Acid l2) = sublistsLoc l1 l2

isKAdjacent :: Int -> DNA -> DNA -> Bool
isKAdjacent k (Acid l1)  (Acid l2) = (l1 /= l2) && take k l2 == drop (length l1 - k) l1 

adjancense :: DNA -> DNA -> Int
adjancense (Acid l1) (Acid l2) = f l1 l2
    where f l1 l2 | l1 `isInfixOf` l2 = length l1
                  | otherwise = f (tail l1) l2

glue :: Int -> DNA -> DNA -> DNA
glue n (Acid l1) (Acid l2) = Acid $ take (length l1 - n) l1 ++ l2

reconstruct :: [DNA] -> DNA
reconstruct dnas = f (let Acid l = head dnas in length l) dnas
    where f _ [] = error "не срослось"
          f _ [dna] = dna
          f n (dna:dnas) = f n $ g n dna dnas
          
          g _ _ [] = []
          g n s (d:ds) | sdn * 2 > n = (glue sdn s d):ds
                       | dsn * 2 > n = (glue dsn d s):ds
                       | otherwise = d:(g n s ds) 
          
            where sdn = adjancense s d
                  dsn = adjancense d s 


profile :: (Num a) => [DNA] -> [(a,a,a,a,a)]
profile dnas = foldl f (replicate (length first) (0,0,0,0,0)) dnas
    where Acid first = head dnas
          f a (Acid l) = zipWith addBase a l

consensus :: (Num a, Ord a) => [(a,a,a,a,a)] -> DNA
consensus p = Acid $ map (\(a,t,g,c,u) -> snd $ maximumBy (\(v1,_) (v2,_) -> compare v1 v2) [(a,Adenin),
                                                                                             (c,Cytosine),
                                                                                             (g,Guanine),
                                                                                             (t,Thymine),
                                                                                             (u,Uracil)]) p

reversePalindromes :: DNA -> Int -> Int -> [(Int,Int)]
reversePalindromes (Acid l) k m = r' l k m 1
    where r' [] _ _ _ = []
          r' l k m n = 
                (map (\s -> (n,length s)) $ filter (\s -> let len = length s
                                                              ac = Acid s
                                                          in len >= k && 
                                                             len <= m &&
                                                             ac == reverseComplement ac) (inits l)) ++ 
                r' (tail l) k m (n+1)

dnaRandomProbLog :: DNA -> Double -> Double
dnaRandomProbLog dna@(Acid l) gc = sum $ map (logBase 10 . f) l 
    where f Adenin = (1 - gc) / 2
          f Thymine = (1 - gc) / 2
          f Guanine = gc / 2
          f Cytosine = gc /2


sharedMotif :: [DNA] -> DNA
sharedMotif dnas = Acid $ fromJust $ find (\sl -> foldl (\b (Acid ds) -> b && (sl `isInfixOf` ds)) True dnas) $ sublists l
    where (Acid l) =  minimumBy (\(Acid l1) (Acid l2) -> compare (length l1) (length l2)) dnas

    
kMerComposition :: Int -> DNA -> [Int]
kMerComposition k dna = map (countSubs str) $ kMers k "ACGT" 
    where str = show dna
          countSubs "" _ = 0
          countSubs str pat | pat `isPrefixOf` str = 1 + countSubs (tail str) pat
                            | otherwise = countSubs (tail str) pat





 


module Rosalind.Problems.SPEC where

import Rosalind.Proteins
import Control.Applicative


main = do f  <- map read <$> lines <$> getContents 
          print (identify f)

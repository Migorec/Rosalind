module Data.Tree.Newick where

import Data.Tree
import Text.Parsec hiding (label)
import Text.Parsec.Prim hiding (label)
import Data.List (intercalate)

showNewick :: Tree String -> String
showNewick t = s t ++ ";"
    where s (Node name []) = name
          s (Node name l) = "(" ++ (intercalate "," $ map s l ) ++ ")" ++ name

readNewick :: String -> Tree String
readNewick str = case (parse tree "" str) of
                    Left err -> error $ show err
                    Right res -> res 
 
tree = do s <- subtree
          char ';'
          return s
                    
subtree = internal <|> leaf  

leaf = do n <- name
          return $ Node (n) []
 
name = many ch
 
ch = char '_' <|> alphaNum
          
internal = do char '('
              branches <- subtree `sepBy` (char ',')
              char ')' 
              n <- name
              return $ Node (n) branches         

                  

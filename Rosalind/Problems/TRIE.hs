module Rosalind.Problems.TRIE where

import Rosalind.List

trieNum (Leaf k) = k
trieNum (Node k _) = k

out :: Trie Int Char -> IO ()
out (Leaf k) = return ()
out (Node k ls) = mapM_ (\(l,t) -> do putStrLn (show k ++ " " ++ (show $ trieNum t) ++ " " ++ [l])
                                      out t) ls

main = do strs <- getContents
          out $ trie (lines strs) 1

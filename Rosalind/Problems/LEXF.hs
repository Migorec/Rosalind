module Rosalind.Problems.LEXF where



solve :: Int -> String -> [String]
solve 0 str = [""]
solve n str = concat $ map (\a -> map (a:) $ solve (n-1) str) str


main = do str1 <- getLine
          str2 <- getLine
          let alph = (concat.words) str1
              n = read str2
              res = solve n alph
          mapM_ putStrLn res

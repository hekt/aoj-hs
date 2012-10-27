import Data.List

adjoinSubseqs :: (Eq a) => [a] -> [[a]]
adjoinSubseqs [] = [[]]
adjoinSubseqs xxs@(x:xs) = inits xxs ++ adjoinSubseqs xs

getSets :: String -> [[Int]]
getSets str = gs $ lines str
    where
      gs (x:xs)
          | n == 0 = []
          | otherwise = (map read $ take n xs): (gs $ drop n xs)
          where n = read x

main = do
  numLists <- fmap getSets getContents
  putStr . unlines $ map (show . maximum . map sum . adjoinSubseqs) numLists
import Data.List

group' :: Eq a => [a] -> [[a]]
group' (x:xs) = grp [x] xs
    where 
      grp a [] = [a]
      grp aas@(a:as) (b:bs)
          | b == a = grp (b:aas) bs
          | otherwise = aas: grp [b] bs

maximumsOfLength :: [[a]] -> [[a]]
maximumsOfLength l = map snd . filter (\x -> fst x == maximumNum pair) $ pair
    where pair = map (\x -> (length x, x)) l
          maximumNum xs = fst $ maximumBy (\(a,_) (b,_) -> compare a b) xs

main = do
  ls <- fmap (lines) getContents
  let nums = map read ls :: [Int]
  putStr . unlines . map (show . head) . maximumsOfLength . group' $ sort nums
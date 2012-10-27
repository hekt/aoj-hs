import Data.List

tradeCoNums :: [(Int, Int)] -> [Int]
tradeCoNums x = map fst x

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

goodConnections :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
goodConnections t l = sortBy (\ a b -> fst a `compare` fst b) . 
                  zip goods $ map (\x -> count (==x) $ tm ++ lm) goods
    where tm = tradeCoNums t
          lm = tradeCoNums l
          goods = filter (flip elem lm) tm

formatting :: String -> ([(Int, Int)], [(Int, Int)])
formatting str = (f thisMonth, f lastMonth)
    where
      f s = map (\ s -> read $ "("++s++")") s
      (thisMonth, lastMonth) = (\ (a,b) -> (a, tail b) )
                               . span (/= "") $ lines str

main = do
  (thisMonth, lastMonth) <- fmap formatting getContents
  putStr . unlines . map (\ (a,b) -> show a ++ " " ++ show b) $ 
       goodConnections thisMonth lastMonth
type Lots = [(Int, Int)]

change :: Lots -> (Int, Int) -> Lots
change lots c = map (chg c) lots
    where chg (a,b) (x,y)
              | x == a = (x, pickup lots b)
              | x == b = (x, pickup lots a)
              | otherwise = (x,y)

pickup :: (Eq k) => [(k, v)] -> k -> v
pickup (x:xs) k
    | fst x == k = snd x
    | otherwise = pickup xs k

main = do
  values <- fmap lines getContents
  let v = read $ head values :: Int
      lots = zip [1..v] [1..v]
      changes = map (\s -> read $ "("++s++")") $ drop 2 values :: [(Int, Int)]
  mapM_ (putStrLn . show . snd) $ foldl (\acc x -> change acc x) lots changes
length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:ns) = 1 + length' ns

shishaGonyu:: (Integral b, RealFrac a) => a -> b
shishaGonyu n = floor $ n + 0.5

total :: [(Int, Int)] -> Int
total [] = 0
total (x:xs) = (fst x * snd x) + total xs

totalAvg :: [(a, Int)] -> Int
totalAvg [] = 0
totalAvg xxs =  shishaGonyu $ sumNum / lenNum
    where
      lenNum = fromIntegral $ length' xxs
      sumNum = fromIntegral $ ta xxs
      ta [] = 0
      ta (x:xs) = snd x + ta xs

getSets :: String -> [(Int, Int)]
getSets str = map (\s -> read $ "("++s++")") $ lines str

main = do
  sets <- fmap getSets getContents
  putStrLn . show $ total sets
  putStrLn . show $ totalAvg sets
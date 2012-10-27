primeList :: Int -> [Int]
primeList n = filter isPrime [0..n]

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = ip (2:[3,5..sqrtN])
    where sqrtN = floor . sqrt $ fromIntegral n
          ip [] = True
          ip (x:xs)
              | n `mod` x == 0 = False
              | otherwise = ip xs

main = do
  numsStr <- getContents
  let nums = map read $ lines numsStr :: [Int]
  putStr . unlines $ map (show . length . primeList) nums
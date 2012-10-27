isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime n = isp (2:[3,5..sqrtN])
    where sqrtN = floor . sqrt $ fromIntegral n
          isp [] = True
          isp (x:xs)
              | n `mod` x == 0 = False
              | otherwise = isp xs

primeLess :: Int -> Int
primeLess n | not (primes == []) = head primes 
            | otherwise = error "too smaller"
    where primes = [m | m <- nums, isPrime m]
          nums | odd n && n > 3 =  [n-2, n-4 .. 2]
               | otherwise = [n-1, n-3 .. 2]

primeMore :: Int -> Int
primeMore n = head [m | m <- nums, isPrime m]
    where nums | odd n = [n+2, n+4 ..]
               | otherwise = [n+1, n+3 ..]

getSets :: String -> [Int]
getSets s = map read $ lines s

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ map (unwords . map show . 
                          (\n -> [primeLess n, primeMore n])) sets
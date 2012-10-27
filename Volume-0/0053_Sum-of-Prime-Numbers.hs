primes :: [Int]
primes = 2: sieve [3,5..]
    where sieve (n:ns) | divisible primes = sieve ns
                       | otherwise        = n: sieve ns
              where sqrtn = floor . sqrt $ fromIntegral n
                    divisible (p:ps) | sqrtn < p      = False
                                     | n `mod` p == 0 = True
                                     | otherwise      = divisible ps

sumOfPrimes :: Int -> Int
sumOfPrimes n = sum $ take n primes

nthPrime :: Int -> Int
nthPrime n = primes !! (n-1)

getSets :: String -> [Int]
getSets str = gs $ lines str
    where
      gs [] = []
      gs ("0":_) = []
      gs (s:ss) = read s: gs ss

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ map (show . sumOfPrimes) sets
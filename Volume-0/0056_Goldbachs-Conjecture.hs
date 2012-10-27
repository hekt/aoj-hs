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

goldbach :: Int -> [(Int, Int)]
goldbach n = [(a,b) | a <- primesFromSmaller, b <- primesFromLarger, a+b == n]
    where primesFromSmaller = [p | p <- (2:[3,5..halfn]), isPrime p]
          primesFromLarger = [p | p <- ([n-1, n-3..halfn]), isPrime p]
          halfn = round $ fromIntegral n / 2
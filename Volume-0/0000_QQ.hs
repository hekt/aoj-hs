qq :: [String]
qq = [ (expStr x y) | x <- [1..9], y <- [1..9] ]
     where expStr a b = show a ++ "x" ++ show b ++ "=" ++ show (a*b)

main = putStr $ unlines qq
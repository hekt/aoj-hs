area :: Int -> Int -> Int
area d n = f (d*n) * d
    where f x = x^2

areas :: Int -> Int -> [Int]
areas mx d = ars 1
    where ars n
              | d*n == (mx-d) = area d n: []
              | otherwise = area d n: ars (n+1)

main = do
  nums <- fmap (map read . lines) getContents
  putStr . unlines $ map (show . sum . areas 600) nums
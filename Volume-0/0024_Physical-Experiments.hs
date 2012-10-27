velocity :: Float -> Float
velocity t = 9.8 * t

distance :: Float -> Float
distance t = 4.9 * (t ^ 2)

dist2time :: Float -> Float
dist2time y = sqrt y / sqrt 4.9

height :: Int -> Int
height n = 5 * n - 5

story2vel :: Int -> Float
story2vel n = velocity . dist2time . fromIntegral $ height n

require :: Float -> Int
require v = req v [1..]
    where 
      req v (n:ns)
          | story2vel n >= v = n
          | otherwise = req v ns

main = do
  nums <- fmap (map read . lines) getContents
  putStr . unlines $ map (show . require) nums
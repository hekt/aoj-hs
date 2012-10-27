balancedWeights :: Int -> [Int]
balancedWeights n = bw [] weights
    where
      weights = reverse $ 1: [2^n | n <- [1..9]]
      bw ws [] = ws
      bw ws (x:xs)
          | n == w = ws
          | (n-w) >= x = bw (x:ws) xs
          | otherwise = bw ws xs
          where w = sum ws

main = do
  nums <- fmap (map read . lines) getContents
  putStr . unlines $ map (unwords . map show . balancedWeights) nums
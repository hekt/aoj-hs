numSeq :: Double -> [Double]
numSeq n = n: seq n False
    where
      seq m isOdd
          | isOdd     = m/3: seq (m/3) False
          | otherwise = m*2: seq (m*2) True

sumOfNthSeq :: Int -> Double -> Double
sumOfNthSeq n m = sum . take n $ numSeq m

formatting :: String -> [Double]
formatting str = map read $ lines str

main = do
  sets <- fmap formatting getContents
  putStr . unlines $ map (show . sumOfNthSeq 10) sets
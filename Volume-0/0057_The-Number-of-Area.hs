splittedArea :: Int -> Int
splittedArea n = 1 + sum [1..n]

formatting :: String -> [Int]
formatting str = map read $ lines str

main = do
  sets <- fmap formatting getContents
  putStr . unlines $ map (show . splittedArea) sets
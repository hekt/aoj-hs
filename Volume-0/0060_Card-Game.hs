leftCards :: [Int] -> [Int]
leftCards used = [x | x <- [1..10], not $ x `elem` used]

lessCards :: Int -> [Int] -> [Int]
lessCards n cards = [x | x <- cards, x < n]

count :: (a -> Bool) -> [a] -> Int
count f xs = length $ filter f xs

probOfUnder21 :: (Int, Int, Int) -> Float
probOfUnder21 (p1, p2, d) = fromIntegral count / 7
    where count = length . lessCards (21-p1-p2) $ leftCards [p1,p2,d]

formatting :: String -> [(Int, Int, Int)]
formatting str = map ((\ [a,b,c] -> (a,b,c)) . map read . words) $ lines str

yesNo :: Bool -> String
yesNo True = "YES"
yesNo _    = "NO"

main = do
  sets <- fmap formatting getContents
  putStr . unlines $ map (yesNo . (>= 0.5) . probOfUnder21) sets

firstDigit :: Int -> Int
firstDigit n = read . (:[]) . head . reverse $ show n

bottoms :: [Int] -> [Int]
bottoms [] = error "empty list"
bottoms (_:[]) = []
bottoms (x:y:zs) = firstDigit (x+y): bottoms (y:zs)

bottommost :: [Int] -> Int
bottommost ns = head . foldl (\ acc x -> x acc) ns 
                $ replicate (length ns - 1) bottoms

formatting :: String -> [[Int]]
formatting str = map f $ lines str
    where f s = map (read . (:[])) s

main = do
  sets <- fmap formatting getContents
  putStr . unlines $ map (show . bottommost) sets

-- test
test0062 = putStr . unlines $ map (show. bottommost) sets
    where sets = formatting "4823108376\n1234567890\n0123456789"
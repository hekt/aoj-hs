type Triangle = (Int, Int, Int)

getSets :: String -> [[Triangle]]
getSets s = sets $ lines s
    where
      strToTri = (\[a,b,c] -> (a,b,c)) . map read . words
      sets [] = []
      sets (x:xs) = (map strToTri $ take n xs): (sets $ drop n xs)
          where n = read x

isRightTriangle :: Triangle -> Bool
isRightTriangle (a,b,c)
    | a^2 + b^2 == c^2 = True
    | a^2 + c^2 == b^2 = True
    | b^2 + c^2 == a^2 = True
    | otherwise = False

main = do
  ssv <- getContents
  let sets = getSets ssv
      yesNo b = case b of True -> "YES"
                          False -> "NO"
  putStr . unlines . map (yesNo . isRightTriangle) $ concat sets
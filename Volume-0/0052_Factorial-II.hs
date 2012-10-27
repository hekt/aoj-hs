lastZeroOfFact :: Int -> Int
lastZeroOfFact n
    | n < 5 = 0
    | otherwise = ndiv5 + lastZeroOfFact ndiv5
    where ndiv5 = floor $ fromIntegral n / 5

getSets :: String -> [Int]
getSets str = gs $ lines str
    where 
      gs [] = []
      gs ("0":_) = []
      gs (s:ss) = read s: gs ss

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ map (show . lastZeroOfFact) sets
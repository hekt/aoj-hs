digits :: Int -> Int
digits = length . show

ssvToIntLists :: String -> [[Int]]
ssvToIntLists = map (map read . words) . lines

main = do
  ssv <- getContents
  let nums = ssvToIntLists ssv
  putStr . unlines $ map (\[x,y] -> show $ digits (x+y)) nums
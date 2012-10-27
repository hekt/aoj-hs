sumOf :: Int -> Int
sumOf n = length [(a,b,c,d) | a <- [0..9], b <- [0..9], 
                  c <- [0..9], d <- [0..9], a+b+c+d == n]

main = do
  numStrs <- getContents
  let nums = map read $ lines numStrs :: [Int]
  putStr . unlines $ map (show . sumOf) nums
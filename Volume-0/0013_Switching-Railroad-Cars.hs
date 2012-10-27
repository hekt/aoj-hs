exits :: [Int] -> [Int]
exits = reverse . snd . foldl (\acc x -> moves acc x) ([], [])
    where moves ([], ys) n = ([n], ys)
          moves (xss@(x:xs) , ys) n
              | n == 0 = (xs, x:ys)
              | otherwise = (n:xss, ys)

main = do
  numStrs <- fmap lines getContents
  let nums = map read numStrs :: [Int]
  putStr . unlines . map show $ exits nums
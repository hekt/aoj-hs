maximum' :: (Ord a) => [a] -> a
maximum' (x:xs) = mx x xs
    where mx y [] = y
          mx y (z:zs) | y > z = mx y zs
                      | otherwise = mx z zs
minimum' :: (Ord a) => [a] -> a
minimum' (x:xs) = mn x xs
    where mn y [] = y
          mn y (z:zs) | y < z = mn y zs
                      | otherwise = mn z zs

difference :: (Num a, Ord a) => [a] -> a
difference ns = maximum' ns - minimum' ns

getSets :: String -> [Float]
getSets str = map read $ lines str

main = do
  sets <- fmap getSets getContents
  putStrLn . show $ difference sets
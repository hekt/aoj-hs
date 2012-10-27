data BloodType = A | B | AB | O deriving (Read, Show, Eq)

count :: BloodType -> [(BloodType, Int)] -> [(BloodType, Int)]
count _ [] = []
count bt (b:bs)
    | bt == fst b = (bt, succ $ snd b): count bt bs
    | otherwise = b: count bt bs

countBlood :: [BloodType] -> [(BloodType, Int)]
countBlood bs = foldl (\acc x -> count x acc) ini bs
    where ini = [(A,0),(B,0),(AB,0),(O,0)] 

getSets :: String -> [BloodType]
getSets str = map (\s -> snd (read $ "("++s++")" :: (Int, BloodType)))
              $ lines str

main = do
  sets <- fmap getSets getContents
  putStr . unlines . map (show . snd) $ countBlood sets
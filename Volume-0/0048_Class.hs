weightClass :: (Num a, Ord a) => a -> String
weightClass n
    | n <= 48           = "light fly"
    | 48 < n && n <= 51 = "fly"
    | 51 < n && n <= 54 = "bantam"
    | 54 < n && n <= 57 = "feather"
    | 57 < n && n <= 60 = "light"
    | 60 < n && n <= 64 = "light welter"
    | 64 < n && n <= 69 = "welter"
    | 69 < n && n <= 75 = "light middle"
    | 75 < n && n <= 81 = "middle"
    | 81 < n && n <= 91 = "light heavy"
    | otherwise = "heavy"

getSets :: String -> [Float]
getSets str = map read $ lines str

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ map weightClass sets
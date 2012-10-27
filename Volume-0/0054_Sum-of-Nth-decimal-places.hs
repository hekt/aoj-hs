decDigits :: (RealFrac a, Show a) => a -> [Int]
decDigits n = map (read . (:[])) . tail . snd . span (/='.') $ show n :: [Int]

sumOfNthDecimalPlaces :: (RealFrac a, Show a) => a -> a -> Int -> Int
sumOfNthDecimalPlaces a b n = sum . take n . decDigits $ a/b

getSets :: String -> [(Float, Float, Int)]
getSets str = gs $ lines str
    where
      gs [] = []
      gs (s:ss) = ((\[a,b,c] -> (read a, read b, read c)) $ words s): gs ss

main = do
  sets <- fmap getSets getContents
  putStr . unlines 
       $ map (show . (\(a,b,n) -> sumOfNthDecimalPlaces a b n)) sets
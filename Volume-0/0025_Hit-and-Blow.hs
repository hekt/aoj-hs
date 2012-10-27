type Numbers4 = (Int, Int, Int, Int)

hit :: Numbers4 -> Numbers4 -> Int
hit a b = sum . map (bool2int . isHit) $ zip a' b'
   where 
      a' = tup42int a
      b' = tup42int b
      isHit (c,d)
          | c == d = True
          | otherwise = False

blow :: Numbers4 -> Numbers4 -> Int
blow as bs@(b1,b2,b3,b4) = sum [1 | b <- [b1,b2,b3,b4], b `elem` as'] 
                            - hit as bs
    where as' = tup42int as

hitAndBlow :: Numbers4 -> Numbers4 -> (Int, Int)
hitAndBlow a b = (hit a b, blow a b)

bool2int :: Bool -> Int
bool2int True = 1
bool2int _ = 0

tup42int :: (a,a,a,a) -> [a]
tup42int (a,b,c,d) = [a,b,c,d]

getSets :: String -> [(Numbers4, Numbers4)]
getSets str = gs $ lines str
    where
      gs [] = []
      gs (a:b:cs) = (str2nums4 a, str2nums4 b): gs cs
      str2nums4 s = (\[a,b,c,d] -> (a,b,c,d))  . map read $ words s

main = do
  sets <- fmap getSets getContents
  let tup2hb (a,b) = hitAndBlow a b
      prtf (h,b) = show h ++ " " ++ show b
  putStr . unlines $ map (prtf . tup2hb) sets
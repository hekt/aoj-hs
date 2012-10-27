splitNum :: String -> [Int]
splitNum str = reverse . map read $ sp str
    where sp [] = []
          sp s
              | len < 9 = s:[]
              | len `mod` 9 == 0 = (take 9 s): (sp $ drop 9 s)
              | otherwise = take (len `mod` 9) s: (sp $ drop (len `mod` 9) s)
              where len = length s

addSplitedNums :: [Int] -> [Int] -> [Int]
addSplitedNums xs ys = zipWith (+) xs' ys'
    where xs' = xs ++ replicate (length ys - length xs) 0
          ys' = ys ++ replicate (length xs - length ys) 0

moveUp :: [Int] -> [Int]
moveUp [] = []
moveUp (x:[]) = [x]
moveUp (x:y:zs)
    | x >= 1000000000 = x': moveUp (y':zs)
    | otherwise = x: moveUp (y:zs)
    where x' = read . tail $ show x
          y' = y + (read . take 1 $ show x)

zeroFill :: Int -> String
zeroFill n = replicate (9 - length n') '0' ++ n'
    where n' = show n

getSets :: [String] -> [(String, String)]
getSets [] = []
getSets (_:[]) = []
getSets (x:y:zs) = (x, y): getSets zs

main = do
  sets <- fmap (getSets . tail . lines) getContents
  let add' (a,b) = moveUp $ addSplitedNums (splitNum a) (splitNum b)
      fill a = (show . head $ reverse a): (map zeroFill . tail . reverse $ a)
      overflow a = if length a > 80 then "overflow" else a
      result = map (overflow . concat . fill . add') sets
  putStrLn $ unlines result
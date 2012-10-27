import Data.List
import Data.Maybe

pts2rankMap :: [Int] -> [(Int, Int)]
pts2rankMap xs = flip zip [1..] . nub . reverse $ sort xs

pickup :: Eq k => k -> [(k,v)] -> Maybe v
pickup _ [] = Nothing
pickup k (x:xs)
    | k == fst x = Just $ snd x
    | otherwise  = pickup k xs

formatting :: String -> ( [(Int, Int)], [Int] )
formatting str = ( map (\s -> read $ "("++s++")") datas
                 , map read nums )
    where 
      (datas, nums) = (\ (a,b) -> (a, tail b)) . span (/="0,0") $ lines str

main = do
  (dataMap, nums) <- fmap formatting getContents
  putStr . unlines 
       $ map (show . fromJust . flip pickup (pts2rankMap $ map snd dataMap) 
              . fromJust . flip pickup dataMap) nums
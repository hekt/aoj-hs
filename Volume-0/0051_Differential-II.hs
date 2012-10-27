import Data.List

genMaxNum :: [Int] -> Int
genMaxNum ns = read . appendNum . reverse $ sort ns

genMinNum :: [Int] -> Int
genMinNum ns = read . appendNum $ sort ns

appendNum :: [Int] -> String
appendNum [] = ""
appendNum (s:ss) = show s ++ appendNum ss

differential :: [Int] -> Int
differential ns = mx - mn
    where mx = genMaxNum ns
          mn = genMinNum ns

getSets :: String -> [[Int]]
getSets str = gs $ lines str
    where
      gs (s:ss) = map (map (read . (:[]))) $ take (read s) ss

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ map (show . differential) sets
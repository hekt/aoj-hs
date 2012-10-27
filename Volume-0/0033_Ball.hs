import Data.Maybe

type Cylinders = [([Int], [Int])]

orderedCylinders :: [Int] -> Maybe Cylinders
orderedCylinders ns = ocs ns [([0], [0])]
    where
      ocs _ [] = Nothing
      ocs [] cs = Just cs
      ocs (a:as) cs = ocs as cs'
          where cs' = possibilities a cs

possibilities :: Int -> Cylinders -> Cylinders
possibilities _ [] = []
possibilities a ((bbs@(b:_),ccs@(c:_)): ps)
    | b < a  && c < a = (bbs', ccs): (bbs, ccs'): possibilities a ps
    | b < a = (bbs', ccs): possibilities a ps
    | c < a = (bbs, ccs'): possibilities a ps
    | otherwise = possibilities a ps
    where bbs' = (a:bbs)
          ccs' = (a:ccs)

getSets :: String -> [[Int]]
getSets str = map (map read . words ) . tail $ lines str

main = do
  sets <- fmap getSets getContents
  let yesNo x = if x == Nothing then "NO"
                else "YES"
  putStr . unlines $ map (yesNo . orderedCylinders) sets
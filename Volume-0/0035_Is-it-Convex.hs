import Text.Regex

type Point = (Float, Float)
type Rect = (Point, Point, Point, Point)

hasConcavity :: Rect -> Bool
hasConcavity (a,b,c,d)
    | isc a b c d = True
    | isc b c d a = True
    | isc c d a b = True
    | isc d a b c = True
    | otherwise = False
    where 
      isc (x1,y1) (x2,y2) (x3,y3) (xp,yp)
          | z1 > 0 && z2 > 0 && z3 > 0 = True
          | z1 < 0 && z2 < 0 && z3 < 0 = True
          | otherwise = False
          where 
            z1 = (x3-x2)*(yp-y2) - (y3-y2)*(xp-x2)
            z2 = (x1-x3)*(yp-y3) - (y1-y3)*(xp-x3)
            z3 = (x2-x1)*(yp-y1) - (y2-y1)*(xp-x1)
                           

getSets :: String -> [Rect]
getSets s = map str2rect $ lines s
    where
      list2pts [] = []
      list2pts (_:[]) = []
      list2pts (x1:x2:xs) = (x1,x2): list2pts xs
      pts2rect (a:b:c:d:_) = (a,b,c,d)
      str2rect = (pts2rect . list2pts . map read . splitRegex (mkRegex ","))

main = do
  sets <- fmap getSets getContents
  let yesNo b = if b == True then "YES"
                else "NO"
  putStr . unlines $ map (yesNo . not . hasConcavity) sets
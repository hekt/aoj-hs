type Point = (Float, Float)
type Line = (Point, Point)

isParallel :: Line -> Line -> Bool
isParallel ((ax1,ay1), (ax2,ay2)) ((bx1,by1), (bx2,by2))
    | tiltA == tiltB = True
    | otherwise = False
    where tiltA = (ay2 - ay1) / (ax2 - ax1)
          tiltB = (by2 - by1) / (bx2 - bx1)

getSets :: String -> [[(Line, Line)]]
getSets s = gs $ lines s
    where 
      gs [] = []
      gs (x:xs) = (map strToLines $ take n xs) : (gs $ drop n xs)
          where n = read x :: Int

strToLines :: String -> (Line, Line)
strToLines s = (\[a,b,c,d,e,f,g,h] -> 
                (((a,b),(c,d)), ((e,f),(g,h))) ) . map read $ words s

main = do
  linesList <- fmap (concat . getSets) getContents
  let yesNo b = case b of True -> "YES"
                          False -> "NO"
  putStr . unlines $ map (yesNo . (\(x,y) -> isParallel x y)) linesList
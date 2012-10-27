type Point = (Float, Float)
type Vector = (Float, Float)
type Rectangle = (Point, Point) -- left bottom, top right

isIntersection :: Rectangle -> Rectangle -> Bool
isIntersection a b = f a b || f b a
    where f x y = or . map (flip isInnerPoint x) . 
                  (\ (a,b,c,d) -> [a,b,c,d]) $ rect2fourPts y

isInnerPoint :: Point -> Rectangle -> Bool
isInnerPoint p rect
    | a <= 0 && b <= 0 && c <= 0 && d <= 0 = True
    | otherwise                            = False
    where (tl, tr, br, bl) = rect2fourPts rect
          a = exProd (pts2vec tl tr) (pts2vec tl p)
          b = exProd (pts2vec tr br) (pts2vec tr p)
          c = exProd (pts2vec br bl) (pts2vec br p)
          d = exProd (pts2vec bl tl) (pts2vec bl p)

rect2fourPts :: Rectangle -> (Point, Point, Point, Point)
rect2fourPts ((x1,y1), (x2,y2)) = 
    (topLeft, topRight, bottomRight, bottomLeft)
    where topLeft     = (x1, y2)
          topRight    = (x2, y2)
          bottomRight = (x2, y1)
          bottomLeft  = (x1, y1)

pts2vec :: Point -> Point -> Vector
pts2vec (x1,y1) (x2,y2) = (x2-x1, y2-y1)

exProd :: Vector -> Vector -> Float
exProd (ax,ay) (bx,by) = ax * by - bx * ay

yesNo :: Bool -> String
yesNo True = "YES"
yesNo _    = "NO"

formatting :: String -> [(Rectangle, Rectangle)]
formatting str = map (f . map read . words) $ lines str
    where f (a:b:c:d:e:f:g:h:_) = ( ((a,b), (c,d)), ((e,f), (g,h)) )

main = do
  sets <- fmap formatting getContents
  putStr . unlines $ map (yesNo . (\(a,b) -> isIntersection a b)) sets
type Triangle = (Point, Point, Point)
type Point = (Int, Int)

dist :: Point -> Point -> Float
dist (x1, y1) (x2, y2) = sqrt $ x+y
    where x = fromIntegral $ (x2-x1)^2
          y = fromIntegral $ (y2-y1)^2


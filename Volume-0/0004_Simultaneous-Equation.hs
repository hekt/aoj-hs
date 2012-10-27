import Text.Printf

-- ax + by = c
-- dx + ey = f

calcX :: Fractional a => a -> a -> a -> a -> a -> a -> a
calcX a b c d e f = (f + negate (e * c / b)) / (d + e * negate a / b)
calcY :: Fractional a => a -> a -> a -> a -> a -> a -> a
calcY a b c d e f = (f + negate (d * c / a)) / (d * negate b / a + e)

main = do
  numStrs <- getContents
  let numLists = map (map read . words) $ lines numStrs :: [[Float]]
      results = map (\[a,b,c,d,e,f] -> xy a b c d e f) numLists
          where xy a b c d e f = (calcX a b c d e f, calcY a b c d e f)
  mapM_ (\(x,y) -> printf "%.3f %.3f\n" x y) results
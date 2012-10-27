import Data.List
import Text.Printf

gcd' :: Integral a => a -> a -> a
gcd' 0 0 = error "invalid"
gcd' a 0 = a
gcd' a b = gcd' b (a `rem` b)

lcm' :: Integral a => a -> a -> a
lcm' a b =  floor $ g * (a'/g) * (b'/g)
    where g = fromIntegral $ gcd' a b
          a' = fromIntegral a
          b' = fromIntegral b

main = do
  ssv <- getContents
  let numLists = map (map read . words) $ lines ssv :: [[Int]]
      results = map (\[x,y] -> (gcd' x y, lcm' x y)) numLists
  mapM_ (\(x,y) -> printf "%d %d\n" x y) results
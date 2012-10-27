import Data.List
import Data.Array

combn :: [a] -> Int -> [[a]]
combn [] _ = []
combn xs 1 = map (:[]) xs
combn (x:xs) n = [x:y | y <- combn xs (n-1)] ++ combn xs n

toBeN :: Int -> Int -> [[Int]]
toBeN n s = f . concat . map permutations $ combn [0..9] n
    where
      f [] = []
      f (x:xs)
          | sum' x == s = x : f xs
          | otherwise  = f xs

sum' :: [Int] -> Int
sum' xxs = f xxs 1
    where 
      f [] _ = 0
      f (x:xs) n = x*n + f xs (n+1)

f lefts@(n:ns) len size total
    | len == size = []
    | f ns (len+1) size (total + n * len)
import Data.List
import Data.Char

mode :: Ord a => [[a]] -> [a]
mode = head . longest . group . sort

longest :: Ord a => [[a]] -> [a]
longest = maximumBy (\x y -> (length x) `compare` (length y))

main = do
  str <- getLine
  let w = words $ map toLower str
      m = mode w
      l = longest w
  putStrLn $ m ++ " " ++ l
import Data.List
import Text.Printf

type Worth = Int
type Weight = Int
type Treasure = (Worth, Weight)

mostWorthComb :: Int -> [Treasure] -> [Treasure]
mostWorthComb limit treasures = 
    maximumBy (\a b -> compare (totalWorth a) (totalWorth b)) 
      . sortBy (\a b -> compare (totalWeight b) (totalWeight a))
      . map (comb []) $ permutations treasures
    where
      comb temp [] = temp
      comb [] (t:ts) = comb [t] ts
      comb temp (t:ts)
          | snd t + totalWeight temp <= limit = comb (t:temp) ts
          | otherwise = comb temp ts

totalWorth :: [Treasure] -> Worth
totalWorth = sum . map fst
totalWeight :: [Treasure] -> Weight
totalWeight = sum . map snd

getSets :: String -> [(Int, (Weight, [Treasure]))]
getSets str = zip [1..] . gs $ lines str
    where
      ss2ts [] = []
      ss2ts (s:ss) = (read $ "("++s++")"): lines2ts ss
      gs [] = []
      gs ("0":_) = []
      gs (w:n:ss) = (w', ss2ts $ take n' ss): (gs $ drop n' ss)
          where w' = read w
                n' = read n

main = do
  sets <- fmap getSets getContents
  let wowe ts = (totalWorth ts, totalWeight ts)
      results = map (\(n,(w,ts)) -> (n, wowe $ mostWorthComb w ts)) sets
      format = (\(n,(wo,we)) -> printf "Case %d:\n%d\n%d" n wo we)
  putStr . unlines $ map format results

-- test that need to because an internal error occurs in GHC on OS X 10.8.0
test0042 = putStr . unlines $ map format results 
    where 
      sets = getSets "50\n5\n60,10\n100,20\n120,30\n210,45\n10,4\n50\n5\n60,10\n100,20\n120,30\n210,45\n10,4"
      wowe ts = (totalWorth ts, totalWeight ts)
      results = map (\(n,(w,ts)) -> (n, wowe $ mostWorthComb w ts)) sets
      format = (\(n,(wo,we)) -> printf "Case %d:\n%d\n%d" n wo we)
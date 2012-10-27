import Text.Regex

type Sections = [KM]
type SectNum = Int
type Speed = Int
type KM = Int

passDist :: KM -> (Speed, Speed) -> KM
passDist kmeter (a,b) = round $ fromIntegral d / 60
    where d = head [a*t | t <- [1..], (a*t + b*t) >= kmeter*60]

meter2section :: Sections -> KM -> SectNum
meter2section s m = head [n | n <- [1..(length s)], (sum $ take n s) >= m]

passSection :: Sections -> (Speed, Speed) -> SectNum
passSection sec spd = meter2section sec $ passDist (sum sec) spd

getSets :: String -> [(Sections, (Speed, Speed))]
getSets str = map gs $ lines str
    where
      gs s = (take 10 s', (s' !! 10, s' !! 11))
          where s' = map read $ splitRegex (mkRegex ",") s

main = do
  sets <- fmap getSets getContents
  putStr . unlines $ map (show . (\(a,b) -> passSection a b)) sets
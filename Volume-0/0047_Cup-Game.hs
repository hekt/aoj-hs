data Cup = A | B | C deriving (Eq, Show, Read)
type CupsWithBall = [(Cup, Bool)]

cupWithBall :: CupsWithBall -> Cup
cupwithBall [] = error "ball not found"
cupWithBall (c:cs)
    | snd c == True = fst c
    | otherwise = cupWithBall cs

pickup :: (Eq k) => k -> [(k,v)] -> v
pickup _ [] = error "Not found"
pickup k (x:xs)
    | k == (fst x) = snd x
    | otherwise = pickup k xs

update :: (Eq k) => (k,v) -> [(k,v)] -> [(k,v)]
update _ [] = []
update (k,v) (x:xs)
    | k == (fst x) = (k,v): update (k,v) xs
    | otherwise = x: update (k,v) xs

change :: (Cup,Cup) -> CupsWithBall -> CupsWithBall
change (x,y) cups = foldl (\acc x -> update x acc) cups [(x,xv), (y,yv)]
    where xv = pickup x cups
          yv = pickup y cups

getSets :: String -> [(Cup,Cup)]
getSets str = map (\s -> read $ "("++s++")") $ lines str

main = do
  sets <- fmap getSets getContents
  let cups = [(A, True), (B, False), (C, False)]
  putStrLn . show . cupWithBall $ foldl (\acc x -> change x acc) cups sets
import Data.Char
import Data.Maybe

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

deaffine :: (Int, Int) -> Char -> Char
deaffine (a,b) c
    | isLower c  = int2char $ (a * char2int c + b) `mod` 26
    | otherwise = c

decipher :: String -> Maybe String
decipher s = dec s [(x,y) | x <- [0..26], y <- [0..26]]
    where 
      dec _ [] = Nothing
      dec s (p:ps)
          | "that" `elem` ws = Just (unwords ws)
          | "this" `elem` ws = Just (unwords ws)
          | otherwise = dec s ps
          where ws = words $ map (deaffine p) s

getSets :: String -> [String]
getSets str = take setNum $ tail l
    where
      l = lines str
      setNum = read $ head l :: Int

main = do
  ciphers <- fmap getSets getContents
  putStrLn . unlines $ map (fromJust . decipher) ciphers
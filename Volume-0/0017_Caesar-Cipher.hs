import Data.Char
import qualified Data.Maybe as Maybe

char2int :: Char -> Int
char2int c = ord c - ord 'a'

int2char :: Int -> Char
int2char n = chr (ord 'a' + n)

shiftChar :: Int -> Char -> Char
shiftChar n c
    | isLower c = int2char $ (char2int c + n) `mod` 26
    | otherwise = c

tryDec :: [String] -> Maybe Int
tryDec strs = dec [(a,b) | a <- strs, b <- [1..26]]
    where
      existWords = ["the", "this", "that"]
      dec [] = Nothing
      dec ((s,n):ts)
          | shifted `elem` existWords = Just n
          | otherwise = dec ts
          where shifted = map (shiftChar n) s

decipher :: [String] -> String
decipher s
    | Maybe.isJust tried = map (shiftChar n) $ unwords s
    | otherwise = "**FAILURE TO DECIPHER**"
    where tried = tryDec s
          n = Maybe.fromJust tried

main = do
  cipheredWordsLists <- fmap (map words . lines) getContents
  putStr . unlines $ map decipher cipheredWordsLists
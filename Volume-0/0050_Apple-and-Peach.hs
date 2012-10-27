replaceStr :: String -> String -> String -> String
replaceStr str1 str2 sentence = unwords' . rep $ words' sentence
    where rep [] = []
          rep (s:ss) | s == str1 = str2: rep ss
                     | otherwise = s: rep ss

words' :: String -> [String]
words' str = wd [] str
    where wd temp [] = []
          wd temp (s:ss) | s == ' '  = temp: " ": wd [] ss
                         | s == '.'  = temp: ".": wd [] ss
                         | s == ','  = temp: ",": wd [] ss
                         | otherwise = wd (temp++[s]) ss

unwords' :: [String] -> String
unwords' [] = ""
unwords' (s:ss) = s ++ unwords' ss

main = do
  sets <- getLine
  putStrLn $ foldl (\acc (s1,s2) -> replaceStr s1 s2 acc) sets [("peach", "apple_"), ("apple", "peach"), ("apple_", "apple")]
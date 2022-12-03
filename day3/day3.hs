import Data.List

main = do
   file <- readFile "input"
   let lis = lines file
   let lst = map (\s -> pri . head . intsect $ splt s) lis
   let by3 = take3 lis
   let map2 = map (\x -> pri $ g3 x) by3
   putStrLn $ show $ sum $ pure sum <*> lst
   putStrLn $ show $ sum $ pure sum <*> map2

letters = ['a'..'z'] ++ ['A'..'Z']

pri :: Char -> Maybe Int
pri c = pure (1+) <*> elemIndex c letters

splt :: String -> (String,String)
splt s = splitAt (div (length s) 2) s

intsect :: (String,String) -> [Char]
intsect (fs,ss) = nub $ intersect fs ss

take3 :: [String] -> [[String]]
take3 = split
   where
      split [] = []
      split xs = take 3 xs : split (drop 3 xs)

g3 :: [String] -> Char
g3 (h:t) = head $ go h t
   where
      go xs []     = xs
      go xs (y:ys) = go (nub $ intersect xs y) ys

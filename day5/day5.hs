import Data.List.Split(splitWhen)
import Data.List(transpose)
import Data.Char (digitToInt,isDigit,isAlpha)

main = do
   file <- readFile "input"
   let [lns,movs] = splitWhen (""==) $ lines file
   let step = [2,6 ..]
   let stackNum = filter (isDigit ) $ last lns
   let indexes = take (length stackNum ) step
   let o = map (\ln-> map (\i-> ln !! (i-1)) indexes) $ init lns
   let stacks = "" : (map (\s-> filter (isAlpha) s) $ transpose o)
   let a = map (\d-> map (filter isDigit) d ) $ map words movs
   let b = map (\ws-> filter (\x->length x > 0) ws) a
   let c = map (\xs-> map (\x->read x::Int) xs) b
   let d = foldl (\st m-> newSt st m 0) stacks c
   let d2 = foldl (\st m-> newSt st m 1) stacks c
   putStrLn $ show $ map head $ tail d 
   putStrLn $ show $ map head $ tail d2 

from, from2 :: String -> Int -> (String,String)
from s q = (drop q s, reverse $ take q s)
from2 s q = (drop q s, take q s)

to :: String -> String -> String
to s t = t ++ s

re :: (Int,String) -> [String] -> [String]
re (ix,new) old = snd newState
   where
      newState = foldl (\(i,ns) st-> if ix == i then (i+1,ns ++ [new]) else (i+1,ns ++ [st]) ) (0,[]) old

newSt :: [String] -> [Int] -> Int -> [String]
newSt s [q,f,t] p = re (t,t') $ re (f,fst f') s
   where
      f' = [from (s !! f) q,from2 (s !! f) q] !! p
      t' = to (s !! t) (snd f')

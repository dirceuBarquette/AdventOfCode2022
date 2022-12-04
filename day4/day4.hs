import Data.List.Split
import Data.List (intersect)

main = do
   file <- readFile "input"
   let lns = lines file
   let split1 = map (\x-> splitWhen  (','==) x) lns
   let split2 = map (\[m,n]-> [toint m,toint n]) split1
   let total  = sum $ map (\[[a,b],[x,y]]-> alltrue [a,b,x,y]) split2
   let total2  = sum $ map (\[[a,b],[x,y]]-> overlap [a,b,x,y]) split2

   putStrLn $ show total
   putStrLn $ show total2

toint :: String -> [Int]
toint = (\[m,n]-> [read m::Int, read n::Int]) . splitWhen ('-'==)

alltrue :: [Int] -> Int
alltrue [a,b,x,y]
   | and [b>=a,y>=x,x>=a,y<=b] = 1
   | and [b>=a,y>=x,a>=x,b<=y] = 1
   | otherwise                 = 0

overlap :: [Int] -> Int
overlap [a,b,x,y]
   | (length $ intersect [a..b] [x..y]) > 0 = 1
   | otherwise                              = 0

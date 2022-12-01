import Data.List

main = do
   file <- readFile "input"
   let lns = lines file
   let toint = map (\s -> if length s > 0 then read s::Int else 0) lns
   let parse = foldl (\(acc,gt) n -> if n > 0 then (acc+n,gt) else (if acc > gt then (0,acc) else (0,gt))) (0,0) toint
   let parse2 = foldl (\(acc,xs) n -> if n > 0 then (acc+n,xs) else (0,acc : xs)) (0,[]) toint
   putStrLn $ show $ snd parse
   putStrLn $ show $ sum . take 3 . reverse . nub $ sort $ snd parse2

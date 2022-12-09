import Data.List (nub)

main :: IO ()
main = do
   file <- readFile "input"
   putStrLn $ show $ eval 4 file
   putStrLn $ show $ eval 14 file

eval :: Int -> String -> Int
eval tk str = go (0,0) str
   where
      go (i,acc) s = if (length . nub $ take tk s) == tk
                        then acc+tk
                        else go (i+1,acc+1) (drop 1 s)

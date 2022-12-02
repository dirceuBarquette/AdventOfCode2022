main = do
   file <- readFile "input"
   let lns = lines file
   let lis = map words lns
   putStrLn $ show $ sum $ map (\s -> score (s !! 0) (s !! 1)) lis
   putStrLn $ show $ sum $ map (\s -> change (str2Code $ s !! 0) (str2Code $ s !! 1)) lis

data Code = A | B | C | X | Y | Z

data Result = IWon | ILost | IDraw

score :: String -> String -> Int
score op me =
   case res opC meC of
      IWon  -> 6 + valmec
      ILost -> valmec
      IDraw -> 3 + valmec
   where
      opC    = str2Code op
      meC    = str2Code me
      valmec = val meC
--A X pedra   1 X Lose
--B Y papel   2 Y draw
--C Z tesoura 3 Z Win
change :: Code -> Code -> Int
change A X = 3
change A Y = 4
change A Z = 8
change B X = 1
change B Y = 5
change B Z = 9
change C X = 2
change C Y = 6
change C Z = 7

res :: Code -> Code -> Result
res A X = IDraw
res A Y = IWon
res A Z = ILost
res B X = ILost
res B Y = IDraw
res B Z = IWon
res C X = IWon
res C Y = ILost
res C Z = IDraw

val :: Code -> Int
val c =
   case c of
    A -> 1
    B -> 2
    C -> 3
    X -> 1
    Y -> 2
    Z -> 3

str2Code :: String -> Code
str2Code c =
   case c of
    "A" -> A
    "B" -> B
    "C" -> C
    "X" -> X
    "Y" -> Y
    "Z" -> Z

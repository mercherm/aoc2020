import Advent
import Data.List (elem, unfoldr)
import Data.List.Split (splitOn)
import Data.Tuple (swap) 

move w l = ((tail w) ++ [head w] ++ [head l], tail l)

step (a, b) =  if head a > head b 
  then move a b
  else swap $ move b a 

play (a, b) = if length a == 0 || length b == 0 then (a ++ b, a, b)
  else play $ step (a, b)

-- there must be a more elegant way to do this
play2 (a, b) p = 
  if length a == 0 || length b == 0 then (a ++ b, a, b)
  else if elem (a,b) p then (a, a, b)
  else if (head a) <= (length $ tail a) && (head b) <= (length $ tail b) then do
    let (v, sA, sB) = play2 (take (head a) (tail a), take (head b) (tail b)) []
    if v == sA then play2 (move a b) ((a, b):p)
    else play2 (swap $ move b a) ((a, b):p)
  else play2 (step (a, b)) ((a, b):p)

score a = sum $ zipWith (*) a (reverse [1..(length a)])

solution inp = do
  let [a, b] = splitOn "\n\n" inp
  let aa = (map toInt (tail $ lines a))
  let bb = (map toInt (tail $ lines b))
  let (v, _,_) = play (aa, bb) 
  print $ score v
  let (v2,_,_) = play2 (aa, bb) []
  print $ score v2

main = do
  inp <- getContents
  solution inp

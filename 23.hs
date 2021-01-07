import Advent
import qualified CircularList as C
import Data.List

-- TODO: write your own CircularList with blackjack and hookers. (and nodes cached by value for fast lookups)
uFocus c = case C.focus c of
  Just f -> f

uRotateTo v c = case C.rotateTo v c of
  Just c0 -> c0
  Nothing -> c

remR n cl = do 
  let xs = tail $ take (n+1) (C.rightElements cl)
  let c0 = C.filterR (`notElem` xs) cl
  (xs, c0)

step cl = do
  let n = 3
  let f = uFocus cl 
  let (xs, c0) = remR n cl
  let (lt, gt) = break (> f) (sort $ C.toList c0)
  let nx = if length lt > 1 then last $ init lt else last gt
  let c1 = foldl (\c v -> C.insertL v c) (uRotateTo nx c0) xs
  C.rotR (uRotateTo f c1)

run cl n = if n == 0 then cl
  else run (step cl) (n - 1)

solution inp = do
  let n = map toInt (map (:[]) (filter (/='\n') inp))
  let nums = C.fromList n
  let final = run nums 100
  print $ tail $ C.rightElements $ uRotateTo 1 final
  -- I don't expect this to finish. because obviously it wouldn't be a challenge.
  let nums2 = C.fromList (n ++ [(1 + maximum n)..(10^6)])
  let final2 = run nums2 (10^7)
  print $ product $ take 2 (tail $ C.rightElements $ uRotateTo 1 final2)

main = do
  inp <- getContents
  solution inp

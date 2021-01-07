import Data.List
import Data.List.Split
import qualified Data.Vector as V

main = do
  inp <- getContents
  solution inp

step v m s = do 
  let i = V.unsafeIndex m v
  if i == -1 then 0
  else s - i

play n s v m = if n == s
  then v
  else play n (s + 1) (step v m s) (V.unsafeUpd m [(v,s)])

solution inp = do
  let nums = map (read :: String -> Int) (splitOn "," inp)
  let len = length nums
  let n = 3 * 10^7
  -- what's the actual necessary size for this?
  -- and why does it keep running OOM?
  let m = V.unsafeUpd (V.fromList (take n $ repeat (-1))) (zip (init nums) [1..])
  --print $ play 2020 len (last nums) m3
  print $ play n len (last nums) m

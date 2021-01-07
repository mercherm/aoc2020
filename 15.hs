import Data.List
import Data.List.Split

main = do
  inp <- getContents
  solution inp

step (x:xs) = case elemIndex x xs of
  Just i -> (1 + i):x:xs
  Nothing -> 0:x:xs 
 
play n l = if n == length l 
  then head l
  else play n $ step l

solution inp = do
  let nums = map (read :: String -> Int) (splitOn "," inp)
  print $ play 2020 (reverse nums)
  -- takes forever. never finishes?
  --print $ play 30000000 (reverse nums)

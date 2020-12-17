import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Map as M

main = do
  inp <- getContents
  solution inp

step (x:xs) = case elemIndex x xs of
  Just i -> (1 + i):x:xs
  Nothing -> 0:x:xs 
 
play l = if 2020 == length l 
  then head l
  else play $ step l

--detectLoop :: [Int] -> Maybe Int
--detectLoop l = find 
--  (\n -> do
--    let (a, b) = splitAt n l
--    isPrefixOf a b
--  ) 
--  [2..(div (length l) 2)]
--
--play2 l = case detectLoop l of
--  Just n -> (take (traceShowId n) l)!!(mod (3*10^7 - length l) n) 
--  Nothing -> play2 $ step l

step2 v m s = case M.lookup v m of
  Just i -> do
    (M.insert v s m, s - i)
  Nothing -> do
    (M.insert v s m, 0)

play2 v m s =  if 3 * 10^7 == s
  then v
  else do 
    let (nM, nV) = step2 v m s
    play2 nV nM (s + 1)

solution inp = do
  let nums = map (read :: String -> Int) (splitOn "," inp)
  let ((m, v), s) = foldl (\((m, _), s) v -> (step2 v m s, s + 1) ) ((M.empty, 0), 1) nums
  print $ play $ reverse nums 
  print $ play2 v m s

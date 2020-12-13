import Data.List.Split
import Data.List
main = do
  inp <- getContents
  solution inp

solution inp = do
  let (t:x:xs) = lines inp
  let ts = read t :: Int
  let ids = map (read :: String -> Int) (map (\v -> if v == "x" then "1" else v) (splitOn "," x))
  let idP = product ids
  let fId = ids!!0
  print ids
  let thingy = map (\id -> case find (\t -> 0 == mod (ts + t) id) [0..] of Just v -> (id, v) ) ids
  --let (_, ot) = find (\(id,_) -> id == fId) thingy 
  print $ sortBy (\(_, a) (_, b) -> compare a b) thingy
  let p2 = find (\t -> all (\(id, i) -> id == 1 || (mod (7 + ts + t + i) id) == 0) (zip ids [0..]) ) (map (*fId) [0..]) -- Note to self: 7 is the nearest offset from ts of the first bus and we know that the answer must be a multiple of the first bus id. oh right. TODO: grab the offset from the thingy for fId
  print p2 

-- am I in a hospital?
-- :readln
-- anyone?
-- am I breathing?
-- how long had I not been breathing?


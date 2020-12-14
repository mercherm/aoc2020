import Control.Exception.Base (assert)
import Data.List.Split
import Data.List
main = do
  inp <- getContents
  solution inp

posRem md rem = if rem >= 0 then rem
  else posRem md (md + rem)

-- https://github.com/copy/aoc-19/blob/master/22.hs#L4
modInv a m =
  let (i, _, g) = gcdExt a m
   in assert (g == 1) i
  where
    gcdExt a 0 = (1, 0, a)
    gcdExt a b =
      let (q, r) = a `quotRem` b
          (s, t, g) = gcdExt b r
       in (t, s - q * t, g)

--euc r m = if gcd r m == 1 
--  then unfoldr 
--    (\(a, b) ->
--      if a == 1 then Nothing 
--      else Just ((b, a, div b a, mod b a) ,(mod b a, a))) 
--    (r, m)
--  else []


-- all bus ids are prime so the combinations of them all have gcd=1

solution inp = do
  let (t:x:xs) = lines inp
  let ts = read t :: Int
  let ids = map (read :: String -> Int) (map (\v -> if v == "x" then "1" else v) (splitOn "," x)) 
  let fId = ids!!0
  let thingy = map (\id -> case find (\t -> 0 == mod (ts + t) id) [0..] of Just v -> (id, v) ) (filter (/=1) ids)
  let ot = case find (\(id,_) -> id == fId) thingy of Just (id, _) -> id 
  let ((bId, bWt):_) = sortBy (\(_, a) (_, b) -> compare a b) thingy
  -- part one:  
  print $ bId * bWt
  
  -- ts = timestamp from input
  -- ot = earliest departure for first bus
  -- fId = first bus Id
  -- we know that the time must be a multiple of the first bus Id
  -- including the input timestamp and offset relative to the first bus might be a mistake here (if somehow the value we're looking for is less than that)
  --let possibleTimes = map (\t -> ts + ot + fId * t) [0..]
  let possibleTimes = map (*fId) [1..]
  
  let indexedIds = filter (\(id, _) -> id /= 1) (zip ids [0..])

  -- (possible?) remainders for each bus id, sorted by largest mod
  let rems = sortBy (\(a, _) (b, _) -> compare b a ) (map (\(md, i) -> (md, posRem md (-i))) indexedIds)
  print (indexedIds, rems)
  let idP = product ids
  print idP 
  -- [(id, index, baseTerm)]
  -- baseTerm being the product of each other modulus 
  let baseTerms = map (\(id, i) -> (id, i, div idP id)) indexedIds
  print baseTerms

  let eucThing = map (\(id, i, b) -> (id, i, b, mod ((modInv b id) * i) id )) baseTerms
  print eucThing

  let minTs = sum $ map (\(_,_, a, b) ->  a * b) eucThing
  print minTs

  let seek a b = if b - a < 0 then b else seek a (b-a)
  
  let actualMinTs = seek idP minTs
  print actualMinTs

-- looks like I've been going on a bad assumption
-- the remainder shouldn't be the index, it should be the modulus minus the index
-- see 13.2.hs for corrections

-- x => (busId - busIndex) (mod busId)


  


  -- now I need to multiply each baseTerm by some product, 
  -- such that p*baseTerm mod id = index
--  let adjBase = map (\(id, i, b) -> (solve b id i)) baseTerms
--  print adjBase 
  
--  let p2 = find (\t -> all (\(id, i) -> 0 == mod (t + i) id) indexedIds ) possibleTimes 
--
--  print p2 


import Control.Exception.Base (assert)
import Data.List.Split
import Data.List

main = do
  inp <- getContents
  solution inp

posRem r m = if r >= 0 then r
  else posRem (m + r) m

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

solution inp = do
  let (_:x:xs) = lines inp
  let ids = map (read :: String -> Int) (map (\v -> if v == "x" then "1" else v) (splitOn "," x)) 
  let idx = filter ((/=1).fst) (zip ids [0..])
  print idx
  
  let idProduct = product ids
  let thisThing = map (\(id, i) -> (id, mod (id - i) id) ) idx
  print thisThing 
  let thatThing = map (\(m, r) -> (div idProduct m, r, m)) thisThing
  print thatThing
  let nextThing = map (\(p, r, m) -> (posRem (modInv p m) m, p, r, m)) thatThing
  print nextThing
  let andThen = map (\(i, p, r, m) -> (i * p * r, r, m) ) nextThing
  print andThen
  let andFinally = foldl (\o (i, _,_) -> o + i) 0 andThen
  print andFinally
  let butWait = andFinally - (div andFinally idProduct) * idProduct
  print butWait

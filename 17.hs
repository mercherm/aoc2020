import qualified Data.Map as M
import Data.Ix
import Debug.Trace

initMap inp = foldl
  (\m (l, y) -> foldl
    (\m (c, x) -> M.insert (x,y,0) ('#' == c) m)
    m
    (zip l [0..])
  )
  M.empty
  (zip (lines inp) [0..])

getVal m p = case M.lookup p m of
  Just v -> v
  Nothing -> False

getCoords3 xR yR zR = concatMap
  (\z -> concatMap
    (\y -> map
      (\x -> (x,y,z))
      (range xR)
    )
    (range yR)
  )
  (range zR)

countNeighbors m (x,y,z) = foldl
  (\c p -> if getVal m p 
    then c+1 
    else c
  ) 
  0
  (filter (/=(x,y,z)) (getCoords3 (x-1, x+1) (y-1, y+1) (z-1, z+1)))

xpd (min, max) = (min - 1, max + 1)

dbgM m xR yR zR = map
  (\z -> foldl
    (\o y -> o ++ (
        map
        (\x -> case getVal m (x, y, z) of
          True -> '#'
          False -> '.'
        )
        (range xR) 
      ) ++ "\n" 
    )
    ("z: " ++ (show z) ++ "\n")
    (range yR)
  )
  (range zR)

step m0 xR yR zR = foldl
  (\m p -> do
    let n = countNeighbors m0 p
    let a = getVal m0 p
    if a == True && (n /= 2 && n /= 3) then M.insert p False m
    else if a == False && n == 3 then M.insert p True m
    else m
  )
  m0
  (getCoords3 xR yR zR)

run m xR yR zR n = do 
  let (nxR, nyR, nzR) = (xpd xR, xpd yR, xpd zR)
  let nM = step m nxR nyR nzR
  -- this hacky nonsense is nonsense.
  -- you have to evaluate the result of the traceId expression, or haskell ignores it. tradeoffs of being lazy, I guess.
  let dM = traceId (unlines $ dbgM nM nxR nyR nzR)
  if (dM!!0) == '?' then M.empty 
  else if n == 0
--  if n == 0
  then m
  else run nM nxR nyR nzR (n-1)

solution inp = do
  putStr $ inp ++ "\n"
  let m = initMap inp
  let l = lines inp
  let result = run m (-1, (length (l!!0))) (-1, (length l)) (0,0) 6
  let count = length $ M.filter (==True) result
  print count

main = do
  inp <- getContents
  solution inp

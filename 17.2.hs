import qualified Data.Map as M
import Data.Ix

initMap inp = foldl
  (\m (l, y) -> foldl
    (\m (c, x) -> M.insert (x,y,0,0) ('#' == c) m)
    m
    (zip l [0..])
  )
  M.empty
  (zip (lines inp) [0..])

getVal m p = case M.lookup p m of
  Just v -> v
  Nothing -> False

getCoords4 xR yR zR wR = concatMap
  (\w -> concatMap
    (\z -> concatMap
      (\y -> map
        (\x -> (x,y,z,w))
        (range xR)
      )
      (range yR)
    )
    (range zR)
  )
  (range wR)

countNeighbors m (x,y,z,w) = foldl
  (\c p -> if getVal m p 
    then c+1 
    else c
  ) 
  0
  (filter 
    (/=(x,y,z,w)) 
    (getCoords4 (x-1, x+1) (y-1, y+1) (z-1, z+1) (w-1, w+1))
  )

xpd (min, max) = (min - 1, max + 1)

step m0 xR yR zR wR = foldl
  (\m p -> do
    let n = countNeighbors m0 p
    let a = getVal m0 p
    if a == True && (n /= 2 && n /= 3) then M.insert p False m
    else if a == False && n == 3 then M.insert p True m
    else m
  )
  m0
  (getCoords4 xR yR zR wR)

run m xR yR zR wR n = do 
  let (nxR, nyR, nzR, nwR) = (xpd xR, xpd yR, xpd zR, xpd wR)
  let nM = step m nxR nyR nzR nwR
  if n == 0
  then m
  else run nM nxR nyR nzR nwR (n-1)

solution inp = do
  putStr $ inp ++ "\n"
  let m = initMap inp
  let l = lines inp
  let result = run m (-1, (length (l!!0))) (-1, (length l)) (0,0) (0,0) 6
  let count = length $ M.filter (==True) result
  print count

main = do
  inp <- getContents
  solution inp

import Advent
import Data.List
import Data.List.Split
import qualified Data.Map as M
import qualified Data.Set as U

rot g = map (\y -> reverse $ map (!!y) g) [0..(length g - 1)]

top g = head g
bot g = last g
lft g = map head g
rgt g = map last g

isMatch a b = 
  if top a == bot b then (0, -1)
  else if rgt a == lft b then (1, 0)
  else if bot a == top b then (0, 1)
  else if lft a == rgt b then (-1, 0)
  else (0, 0)

parse inp = map (\(x:xs) -> (init $ last $ splitOn " " x, xs)) $ 
  map lines $ 
  splitOn "\n\n" inp

unBorder g = map ((init).(tail)) $ init $ tail g

matches a (id, g) = filter (\(i, _, m) -> i /= id && m /= (0,0)) $ 
  map (\(i, b) -> (i, b, isMatch g b)) a

perms g = concat [[x, reverse x] | x <- take 4 $ iterate rot g]

allPerms a = concatMap 
  (\(id, g) -> 
    map (\b -> (id,b)) $ 
    perms g)
  a

lookupWithDefault i def xs = case lookup i (zip [0..] xs) of
  Just x -> x
  Nothing -> def

makeImage p s = img where
  foo m  pos (id, g) = if elem g m then m
    else foldl (\m (i, b, d) -> foo m (addTup pos d) (i, b)) (M.insert pos g m) $ 
      matches p (id, g)
  m = foo M.empty (0,0) s
  (minX, minY) = minimum $ M.keys m
  (maxX, maxY) = maximum $ M.keys m
  img = foldl (\o y ->
      o ++ unlines (foldl (\o x -> case M.lookup (x, y) m of
        Just g -> map 
          (\(row, k) -> (lookupWithDefault k "" o) ++ row) $ 
          zip (unBorder g) [0..]
        Nothing -> o
      ) [] [minX..maxX])
    ) "" [minY..maxY]

mnstr = [
    "                  # ",
    "#    ##    ##    ###",
    " #  #  #  #  #  #   "
  ]
mL = length $ filter (=='#') $ unlines mnstr
mH = length mnstr
mW = length $ mnstr!!0

hasMonster m = mL == length m

findMonster g = foldl 
  (\o (r,y) -> foldl 
    (\o ((c,p),x) -> if c == (mnstr!!y)!!x then p:o else o ) 
    o 
    (zip r [0..])) 
  [] 
  (zip g [0..])

searchForMonsters g = concat $ 
    filter hasMonster $
    map findMonster $ 
    concatMap (\a -> transpose $ map (divvy mW 1) a) $ 
    divvy mH 1 (attachCoords g)

paintMonsters (g, crds) = unlines $ map 
  (\(r,y) -> map 
    (\(c,x) -> if elem (x,y) crds then '@' 
      else if c == '.' then '~'
      else if c == '#' then '^'
      else c)
    (zip r [0..]))
  (zip g [0..])

attachCoords g = map 
  (\(r, y) -> map 
    (\(c, x) -> (c, (x,y))) 
    (zip r [0..])) 
  (zip g [0..])

charCount c s = length $ filter (==c) s

solution inp = do
  let a = parse inp
  let p = allPerms a
  let m = makeImage p (head p)
  let pM = perms $ lines m
  let mnstrSpots = head $ map paintMonsters $ filter ((>0).(length).snd) $ map (\g -> (g, searchForMonsters g)) pM
  let octos = charCount '^' mnstrSpots
  putStr $ mnstrSpots
  print octos

main = do
  inp <- getContents
  solution inp

{- 

12  21 
43  34 
       
41  14 
32  23 
       
34  43 
21  12 
       
23  32 
14  41 

-}

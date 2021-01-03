import Advent
import Data.List
import Data.List.Split
import Debug.Trace
import qualified Data.Map as M

iSqrt n = ceiling $ sqrt $ fromIntegral n

rot g = map (\y -> reverse $ map (!!y) g) [0..(length g - 1)]

top g = head g
bot g = last g
lft g = map head g
rgt g = map last g

isMatch a b = 
  if top a == bot b then 1
  else if rgt a == lft b then 2
  else if bot a == top b then 3
  else if lft a == rgt b then 4
  else 0 
  
match a b = filter ((/=0).snd) $
  map (\(c,i) -> ((c,i), isMatch a c)) $
  zip (concat [[x, reverse x] | x <- take 4 $ iterate rot b]) [1..]

matches a (id, g) = map (\(i, a) -> (i, head a)) $ 
  filter (\((i,_),a) -> i /= id && 0 < length a) $ 
  map (\(i, c) -> (i, match g c)) a

parse inp = map (\(x:xs) -> (init $ last $ splitOn " " x, xs)) $ 
  map lines $ 
  splitOn "\n\n" inp

unBorder g = map ((init).(tail)) $ init $ tail g
--unBorder g = map (init $ tail) $ init $ tail g

getCorner m = case find ((==2).(length).(snd)) m of
  Just x -> x
  Nothing -> (("",0), [])

getId l id = case find (\((i,_),_) -> i==id) l of
  Just x -> x
  Nothing -> (("",0), [])

foo l mp (id, matches) (x,y) = if elem id mp 
  then mp
  else foldl (\m (i, (g, d)) -> case d of
      1 -> foo l m (getId l i) (x, y - 1)
      2 -> foo l m (getId l i) (x + 1, y)
      3 -> foo l m (getId l i) (x, y + 1)
      4 -> foo l m (getId l i) (x - 1, y)
    ) (M.insert (x,y) id mp) matches  

-- first thought: construct a 2d array of tuples representing a grid of tiles
-- then concat the colums in each row, then concat the rows
-- don't forget to strip out the borderS
makeImage m = do
  let corn = getCorner m 
  map (\(p, id) -> ) $ 
  foo m M.empty corn (0,0) 

solution inp = do
  let a = parse inp
  let m = map (\(id, g) -> ((id,0), matches a (id, g))) a
  print $ a
  print $ m
  print $ product $ map ((toInt).fst) $ filter ((==2).(length).(snd)) m
  print $ makeImage m

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

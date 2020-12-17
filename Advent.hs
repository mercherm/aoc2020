module Advent (
  toInt,
  combinations,
  pivot
) where

combinations a b = if a == 1 
    then map (\x -> [x]) b 
    else concatMap (\x -> map (\y -> concat [[x], y]) (combinations (a-1) (tail b)) ) b

toInt v = read v :: Int

-- like zip but flexibler
-- will bork if lists not all same size
pivot :: [[a]] -> [[a]]
pivot l = foldl
  (\o i -> o ++ [(map (\sl -> sl!!i) l)])
  []
  [0..((length $ l!!0) - 1)]

module Advent (
  toInt,
  combinations,
  pivot,
  addTup,
  cartesianProduct,
  modInv
) where

import Control.Exception.Base (assert)

cartesianProduct f xs ys = [f x y | x <- xs, y <- ys]

addTup (a, b) (c, d) = (a + c, b + d)

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

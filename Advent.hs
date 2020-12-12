module Advent (
  combinations
) where

combinations a b = if a == 1 
    then map (\x -> [x]) b 
    else concatMap (\x -> map (\y -> concat [[x], y]) (combinations (a-1) (tail b)) ) b

import Data.List

main = do
    contents <- getContents
    print (solution (parse contents))

combinations a b = if a == 1 
    then map (\x -> [x]) b 
    else concatMap (\x -> map (\y -> concat [[x], y]) (combinations (a-1) (tail b)) ) b

parse dat = map read (lines dat)

find2020 comb = find (\(a, b, c) -> a == 2020) (map (\x -> (sum x, product x, x)) comb)

solution inp = (find2020 (combinations 2 inp), find2020 (combinations 3 inp))


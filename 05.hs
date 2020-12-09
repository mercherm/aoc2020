import Data.List

main = do
  inp <- getContents
  print (solution (parse inp))

parse inp = map (\ln -> splitAt 7 ln) (lines inp)

binSpace :: Bool -> (Int, Int) -> (Int, Int)
binSpace top (min, max) = if top 
  then (min + mid + 1, max) 
  else (min, max - mid - 1)
  where mid = div (max-min) 2

doIt isTop x range = (\(min, max) -> min) $ foldl (\r c -> binSpace (isTop c) r) range x

getSeatLocations inp = map (\(row, col) -> (doIt (=='B') row (0, 127), doIt (=='R') col (0, 7))) inp

sortedSeats inp = sortBy (\(r0, c0, s0) (r1, c1, s1) -> compare s1 s0 ) (map (\(r, c) -> (r, c, r * 8 + c)) (getSeatLocations inp))

solution inp = (\(r,c,s) -> s - 1) $ foldl (\(r0, c0, s0) (r1, c1, s1) -> if s0 == 0 || s1 == s0 - 1
    then (r1, c1, s1)
    else (r0, c0, s0)
  ) (0, 0, 0) (sortedSeats inp)

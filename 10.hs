import Data.List
import Data.List.Split

main = do
  inp <- getContents
  solution inp

p 4 = 7
p 3 = 4
p 2 = 2
p 1 = 1
p n = n -- todo: Maths this properlike

solution inp = do
  let nums = map (read :: String -> Int) (lines inp)
  let dev = 3 + maximum nums
  let srt = sort (0:dev:nums) -- add the outlet and the device
  let pairs = divvy 2 1 srt
  let diffs = map (\[a,b] -> b - a) pairs
  -- I'd prefer a more clever way to do these:
  let ones = length $ filter (==1) diffs
  let threes = length $ filter (==3) diffs
  let thingy = filter (>0) (map length (splitWhen (==3) diffs))
  print $ show (ones * threes)
  print $ product (map p thingy)

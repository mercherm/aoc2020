import Data.List
import Data.List.Split
import Advent

main = do
  inp <- getContents
  solution inp

test a = not $ elem v sums where
  (l, v:_) = splitAt 25 a
  sums = (map sum (combinations 2 l))

solution inp = do
  let nums = map (read :: String -> Int) (lines inp)
  case find test (divvy 26 1 nums) of
    Just v -> do
      let (_, n:_) = splitAt 25 v
      print n
      let field = concatMap (\n -> divvy n 1 nums) [2..]
      case find (\s -> n == sum s) field of
        Just b -> print $ (minimum b) + (maximum b)
  

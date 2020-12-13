import Data.List

toInt a = read a :: Int

main = do
  inp <- getContents
  solution inp

rotL (a, b) = (b, -a)
rotR (a, b) = (-b, a)

scale (a, b) s = (a * s, b * s)
add (a, b) (c, d) = (a + c, b + d)

move (pos, dir) (op, val) = case op of
  'N' -> (add pos (0, -val), dir) 
  'S' -> (add pos (0, val), dir)
  'E' -> (add pos (val, 0), dir)
  'W' -> (add pos (-val, 0), dir)
  'L' -> (pos, (iterate rotL dir)!!(div val 90))
  'R' -> (pos, (iterate rotR dir)!!(div val 90))
  'F' -> (add pos (scale dir val), dir)

solution inp = do
  let ops = map (\(x:xs) -> (x, read xs :: Int)) (lines inp)
  let ((x,y), dir) = foldl move ((0, 0), (1, 0)) ops
  print $ (abs x) + (abs y)

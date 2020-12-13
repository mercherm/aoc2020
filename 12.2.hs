import Data.List

main = do
  inp <- getContents
  solution inp

rotL (a, b) = (b, -a)
rotR (a, b) = (-b, a)

scale (a, b) s = (a * s, b * s)
add (a, b) (c, d) = (a + c, b + d)

move (pos, wpt) (op, val) = case op of
  'N' -> (pos, add wpt (0, -val)) 
  'S' -> (pos, add wpt (0, val))
  'E' -> (pos, add wpt (val, 0))
  'W' -> (pos, add wpt (-val, 0))
  'L' -> (pos, (iterate rotL wpt)!!(div val 90))
  'R' -> (pos, (iterate rotR wpt)!!(div val 90))
  'F' -> (add pos (scale wpt val), wpt)

solution inp = do
  let ops = map (\(x:xs) -> (x, read xs :: Int)) (lines inp)
  let ((x,y), wpt) = foldl move ((0, 0), (10, -1)) ops
  print $ (abs x) + (abs y)

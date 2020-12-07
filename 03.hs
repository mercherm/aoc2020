main = do
  inp <- getContents
  print (solution inp)

testSlope arr wid (dx, dy) =
  length (filter (\(row, y) -> (mod y dy == 0) && row!!(mod (y * dx) wid) == '#' ) arr) 

solution inp = (\x -> (product x, x)) $ 
  map (\s ->
    testSlope 
      (zip (lines inp) [0..])
      (length (head (lines inp)))
      s
  ) [(1,1), (3,1), (5,1), (7,1), (1,2)]


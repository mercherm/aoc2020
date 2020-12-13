import Data.List

main = do
  inp <- getContents
  solution inp

findSeatOrEdge :: [[Char]] -> (Int, Int) -> (Int, Int) -> Bool
findSeatOrEdge mp (x, y) (dX, dY) = do
  let nx = x + dX; ny = y + dY
  if nx < 0 || ny < 0 || ny >= (length mp) || nx >= (length (mp!!0)) 
  then False
  else if (mp!!ny)!!nx == '.' then findSeatOrEdge mp (nx, ny) (dX, dY) 
  else (mp!!ny)!!nx == '#'

getCount pos mp = length $ filter 
  (findSeatOrEdge mp pos) 
  [
    (-1, -1), (0, -1), (1, -1),
    (-1,  0),          (1, 0),
    (-1,  1), (0,  1), (1, 1)
  ]

doGen mp = 
  map 
  (\(row, y) -> 
    map 
    (\(col, x) -> 
      do
        let count = getCount (x, y) mp
        if col == 'L' && count == 0 then '#'
        else if col == '#' && count > 4 then 'L'
        else col
    ) 
    (zip row [0..])
  )
  (zip mp [0..])

stabilize mp = do 
  let next = doGen mp
  if (unlines next) == (unlines mp) then next
  else stabilize next

solution inp = do
  let mp = lines inp
  let final = unlines (stabilize mp)
  putStr $ final
  print $ length (filter (=='#') final)

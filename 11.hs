import Data.List

main = do
  inp <- getContents
  solution inp

getCount (x,y) mp = length $ filter 
  (\(aX, aY) -> 
    aX >= 0 && aY >= 0 && aY < (length mp) && aX < (length (mp!!0)) 
    && '#' == (mp!!aY)!!aX
  ) 
  [
    (x-1, y-1), (x, y-1), (x+1, y-1),
    (x-1, y),             (x+1, y),
    (x-1, y+1), (x, y+1), (x+1, y+1)
  ]

doGen mp = 
  map 
  (\(row, y) -> 
    map 
    (\(col, x) -> 
      do
        let count = getCount (x, y) mp
        if col == 'L' && count == 0 then '#'
        else if col == '#' && count > 3 then 'L'
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

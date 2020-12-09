import qualified Data.Set as Set
import qualified Data.List.Split as S
import Data.List (intersect)

main = do
  inp <- getContents
  print (solution inp)

partOne inp = sum $ map (\b -> Set.size (Set.fromList (filter (/='\n') b))) (S.splitOn "\n\n" inp)

alpha = ['a'..'z']

partTwo inp = sum $
  map 
    length 
    (map 
      (\g -> foldl intersect alpha (S.splitOn "\n" g))
      (S.splitOn "\n\n" inp)
    )

solution inp = (partOne inp, partTwo inp)

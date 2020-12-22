import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import qualified Data.List.Split as S
import Data.List
import qualified Data.Set as U
import qualified Data.Map as M

type MParser = Parsec Void [Char]
type Food = ([String], [String])

foodPsr :: MParser Food
foodPsr = try $ do
  ingredients <- sepEndBy (some letterChar) (char ' ')
  string "(contains "
  alergens <- sepBy (some letterChar) (string ", ")
  char ')'
  return (ingredients, alergens)

filePsr :: MParser [Food]
filePsr = try $ do
  foods <- sepEndBy foodPsr (char '\n')
  return foods

-- this is so ugly
doMap foods = do
  let ings = U.fromList $ concatMap (fst) foods
  let alers = U.fromList $ concatMap (snd) foods
  let thing = concatMap (\(ing, aler) -> map (\a -> (a, ing)) aler) foods
  let thang = foldl (\m (aler, ings) -> case M.lookup aler m of
        Just x -> M.insert aler (intersect x ings) m
        Nothing -> M.insert aler ings m
        ) M.empty thing
  let thong = M.foldr (\o ings -> union o ings) [] thang 
  (thang, (U.toList ings) \\ thong)

solution inp = case parse filePsr "" inp of
  Right foods -> do
    let f = sortOn ((length).snd) foods
    let (thang, mp) = doMap f
    print $ sum $ map (\i -> length $ intersect mp i) (map fst f)
    print thang
  Left e -> print e

{- this is how to coding, yes?
"eggs",["ntft","",""]
"fish",["","","nhx",""]
"nuts",["kfxr"]
"peanuts",["xmhsbd",""]
"sesame",["rrjb","",""]
"shellfish",["","xzhxj"]
"soy",["chbtp",""]
"wheat",["cqvc","",""]
-}

main = do
  inp <- getContents
  solution inp

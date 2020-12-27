import Advent (addTup)
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.List

type MParser = Parsec Void [Char]

mvParser :: MParser String
mvParser = try $ do
  dir <- 
    string "se" <|> 
    string "sw" <|>
    string "ne" <|>
    string "nw" <|>
    string "e" <|> 
    string "w"
  return dir

navParser :: MParser [String]
navParser = try $ do
  mvs <- some mvParser
  return mvs

prsLine line = case parse navParser "" line of
  Right x -> map dirTup x
  Left e -> []

dirTup dir = case dir of
  "se" -> ( 1,  1)
  "sw" -> (-1,  1)
  "ne" -> ( 1, -1)
  "nw" -> (-1, -1)
  "e"  -> ( 2,  0)
  "w"  -> (-2,  0)

neighbors p  = map (addTup p) 
  [( 1,  1),
   (-1,  1),
   ( 1, -1),
   (-1, -1),
   ( 2,  0),
   (-2,  0)]

gen m = map fst $
  filter (\(p, v) -> (elem p m && elem v [1,2]) || (notElem p m && v == 2)) $
  map (\l -> (l!!0, length l)) $ 
  group $ sort $ concatMap neighbors m

solution inp = do
  let a = map (!!0) $
          filter (\l -> 1 == mod (length l) 2) $
          group $ sort $ map (foldl addTup (0,0)) $ 
          map prsLine $ lines inp
  -- part one
  print $ length a 
  -- part two
  let b = iterate gen a
  print $ length $ b !! 100

main = do
  inp <- getContents
  solution inp

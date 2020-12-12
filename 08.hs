import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S
import Data.List 
import Data.Void

type MParser = Parsec Void String
type Operation = (String, Int)

main = do
  inp <- getContents
  solution inp

opParser :: MParser Operation
opParser = try $ do
  op <- some letterChar
  char ' '
  optional (char '+')
  arg <- some (char '-' <|> digitChar)
  return (op :: String, read arg :: Int)

fileParser :: MParser [Operation]
fileParser = try $ do
  ops <- sepEndBy1 opParser (char '\n')
  return ops

getOps inp = case parse fileParser "" inp of
  Left e -> [] :: [Operation]
  Right ops -> ops

tur :: [Int] -> Int -> Int -> [Int] -> [String] -> (Int, Int)
tur args acc cur hst ops = do
  let op = ops !! cur
  let arg = args !! cur
  if cur == length ops 
  then (0, acc) -- Terminated Normally
  else if (elem cur hst)
  then (-1, acc) -- Terminated due to repetition
  else case op of
    "nop" -> tur args acc (cur + 1) (cur:hst) ops
    "jmp" -> tur args acc (cur + arg) (cur:hst) ops
    "acc" -> tur args (acc + arg) (cur + 1) (cur:hst) ops

-- Is there a better way to do this?
-- set the item at i to value v in list lst
setAt i v lst = nLst where
  (x,_:ys) = splitAt i lst
  nLst = x ++ [v] ++ ys

doTheNopJmpHop :: [String] -> [[String]]
doTheNopJmpHop ops = do
  let nops = findIndices (=="nop") ops
  let jmps = findIndices (=="jmp") ops
  (map (\i -> setAt i "jmp" ops) nops) ++ 
    (map (\i -> setAt i "nop" ops) jmps)
 
-- might be a good idea to unzip the ops and the op arguments
-- makes it easier to change the ops without changing the args
solution inp = do 
  let (ops, args) = unzip $ getOps inp
  let swps = doTheNopJmpHop ops
  let results = map (tur args 0 0 []) swps 
  print $ tur args 0 0 [] ops
  print $ filter ((==0).fst) results


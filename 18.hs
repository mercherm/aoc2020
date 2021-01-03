import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Tree as T
import Debug.Trace

type MParser = Parsec Void [Char]
type Atree = T.Tree [Char]

parens = between (char '(') (char ')')

unwp op v = case v of 
  Just x -> x
  Nothing -> if op == "+" then 0 else 1

litPsr :: MParser Int
litPsr = try $ do
  n <- some digitChar
  return $ toInt n

exprPsr :: MParser Int
exprPsr = try $ do
  left <- litPsr <|> (parens exprPsr)
  op <- optional ((string "+") <|> (string "*"))
  right <- optional exprPsr
  return $ case op of 
    Just x -> case x of 
      "+" -> left + (unwp x right)
      "*" -> left * (unwp x right)
    Nothing -> left

flp inp = map (\c ->
  if c == '(' then ')'
  else if c == ')' then '('
  else c) (reverse inp)

process inp = case parse exprPsr "" (flp inp) of
  Right t -> t
  Left e -> 0

solution xs = sum $ traceShowId xs

main = do
  inp <- getContents
  -- the incorrect (but effective) way to ignore all whitespace:
  let res = map process $ lines (filter (/=' ') inp)
  --putStr $ T.drawForest res
  print $ solution res

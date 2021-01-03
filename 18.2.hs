import Advent
import Data.List.Split
import qualified NanoParsec as N 

run :: String -> N.Expr
run = N.runParser expr

expr :: N.Parser N.Expr
expr = term `N.chainl1` N.mulop

term :: N.Parser N.Expr
term = factor `N.chainl1` N.addop

factor :: N.Parser N.Expr
factor = N.option N.int (N.parens expr)

solution inp = do
  let lines = init $ splitOn "\n" (filter (/=' ') inp)
  let exps = map run lines
  let res = sum $ map N.eval exps
  print res


main = do
  inp <- getContents
  solution inp

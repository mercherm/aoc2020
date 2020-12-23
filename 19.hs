import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Data.List
import qualified Data.List.Split as S
import qualified Data.Map as M

type MParser = Parsec Void [Char]
type SubRule = [[Int]]
data Rule = SubRule [[Int]] | CharRule Char deriving (Show)
type IdRule = (Int, Rule)

quoted = between (char '"') (char '"')

subPsr :: MParser [Int]
subPsr = try $ do
  subs <- sepEndBy1 (some digitChar) (char ' ')
  return $ map toInt subs

subRulePsr :: MParser Rule
subRulePsr = try $ do
  rules <- sepBy1 subPsr (string "| ")
  return (SubRule rules)

charRulePsr :: MParser Rule
charRulePsr = try $ do
  c <- quoted letterChar
  return (CharRule c)

rlPsr :: MParser Rule
rlPsr = try $ do
  rule <- subRulePsr <|> charRulePsr
  return rule
 
rulePsr :: MParser IdRule
rulePsr = try $ do
  id <- some digitChar
  string ": "
  rule <- rlPsr
  return (toInt id, rule)

msgPsr :: MParser String  
msgPsr = try $ do
  msg <- some letterChar
  return msg

rulesPsr :: MParser [IdRule]
rulesPsr = try $ do
  rules <- sepEndBy rulePsr (char '\n')
  return rules

isCharRule (CharRule _) = True
isCharRule _ = False
isSubRule (SubRule _) = True
isSubRule _ = False

getCharVal (CharRule x) = x
getSubVal (SubRule x) = x

cProd f xs ys = [f x y | x <- xs, y <- ys]
fSnds f l = map (\(a,b) -> (a, f b)) l

process m id = foo id where
  foo k = case M.lookup k m of
    Just x -> concatMap (\l -> foldl (\b i -> cProd (++) b (foo i)) [[]] l ) x
    Nothing -> [[k]]

process2 m mx = foo 0 0 where
  foo k d = if d == mx then [[k]] else case M.lookup k m of
    Just x -> concatMap (\l -> foldl (\b i -> cProd (++) b (foo i (d+1))) [[]] l ) x
    Nothing -> [[k]]

solution inp = do
  let [rs, ms] = S.splitOn "\n\n" inp
  case parse rulesPsr "" rs of
    Right x -> do
      let chars = M.fromList $ fSnds getCharVal (filter ((isCharRule).snd) x)
      let subs = M.fromList $ fSnds getSubVal (filter ((isSubRule).snd) x)
      let strs = map (map (chars M.!)) (process subs 0)
      let msgs = lines ms
      let mx = maximum (map length msgs)
      let matches = intersect msgs strs
      print $ length matches
      let p2Subs = M.delete 8 (M.delete 11 subs)
      let p2Chars = M.insert 8 'c' (M.insert 11 'd' chars)
      -- I don't expect this to work. 
      let p2Strs = map (map (p2Chars M.!)) (process2 p2Subs mx)
      print p2Strs
      --let p2Matches = intersect msgs p2Strs
      --print $ length p2Matches

main = do
  inp <- getContents
  solution inp

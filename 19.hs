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

every f xs = length xs == length (filter f xs)

--  8: 42 | 8
-- 11: 42 31 | 42 11 31
--  0: 8 11
--  0: [42,...,42] [42,...,42,31,...,31]
matchesZero fortyTwo thirtyOne str = alright where
  len = length $ fortyTwo!!0 
  chunks = S.chunksOf len str
  (left, right) = span (`elem` fortyTwo) chunks
  lenL = length left
  lenR = length right
  alright = lenL >= 2 && lenR > 0 && lenL > lenR && every (`elem` thirtyOne) right

solution inp = do
  let [rs, ms] = S.splitOn "\n\n" inp
  case parse rulesPsr "" rs of
    Right x -> do
      let msgs = lines ms
      let chars = M.fromList $ fSnds getCharVal (filter ((isCharRule).snd) x)
      let subs = M.fromList $ fSnds getSubVal (filter ((isSubRule).snd) x)
      -- this is my (far too slow) solution to part one:
      --let strs = map (map (chars M.!)) (process subs 0)
      --let matches = intersect msgs strs
      --print $ length matches
      let fortyTwo = map (map (chars M.!)) (process subs 42)
      let thirtyOne = map (map (chars M.!)) (process subs 31)
      let matches2 = filter (matchesZero fortyTwo thirtyOne) msgs
      -- debug messages for posterity:
      --print $ map (span (`elem` fortyTwo)) (map (S.chunksOf 8) matches2)
      --putStr $ unlines matches2
      print $ length $ matches2

main = do
  inp <- getContents
  solution inp

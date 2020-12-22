import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Debug.Trace
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

-- this is wrong... needs to return a list of strings
-- also how to case by type?
process m = possible where
  step o k = case M.lookup k m of
    Just CharRule c -> [c]
    Just SubRule x -> concatMap (\l -> o ++ (foldl (\v k -> step v k) "" l) ) x
  possible = step [] 0

-- mebbe I should build a tree?
-- 0
-- | 4 -> a
-- | 1 -> 2 -> 4 4 -> aa
--      L 3 -> 4 5 -> ab
-- -   -> 3
--      L 2 
{-
  function thingy (map) {
    function build(o, k) {
      let v = map[k];
      if (typeof v == "String") return o.concat(v);
      return build(
    }
    return map[0].reduce((o, k) =>  , []);
  }
-}

solution inp = do
  let [rs, ms] = S.splitOn "\n\n" inp
  case parse rulesPsr "" rs of
    Right x -> do
      --let m = M.fromList x
      print $ process x
      --print $ M.fromList x  

main = do
  inp <- getContents
  solution inp

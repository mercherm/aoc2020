import Advent
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Data.Void
import Debug.Trace
import qualified Data.List.Split as S

type MParser = Parsec Void [Char]
type SubRule = [[Int]]
data Rule = SubRule [[Int]] | CharRule Char deriving (Show)
type IdRule = (String, Rule)

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
  return (id, rule)

msgPsr :: MParser String  
msgPsr = try $ do
  msg <- some letterChar
  return msg

rulesPsr :: MParser [IdRule]
rulesPsr = try $ do
  rules <- sepEndBy rulePsr (char '\n')
  return rules

solution inp = do
  let [rs, ms] = S.splitOn "\n\n" inp
  print $ parse rulesPsr "" rs 

main = do
  inp <- getContents
  solution inp

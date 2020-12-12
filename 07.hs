import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Set as S 
import qualified Data.Char as C
import Data.Void
import qualified Data.Map as Map
import Data.Either

type MParser = Parsec Void [Char]
type Bag = [Char]
type IBag = (Int, Bag)
type BBag = (Bag, [IBag])
type FBag = (Bag, [Bag])
type MBag = Map.Map Bag [Bag]

main = do
  inp <- getContents
  solution inp

emptyBagParser :: MParser [IBag]
emptyBagParser = try $ do
  string "no other bags"
  return []

bagParser :: MParser Bag 
bagParser = try $ do
  key <- someTill anyChar (string " bag") 
  optional (char 's')
  return key

bagWithQuantParser :: MParser IBag
bagWithQuantParser = try $ do
  quant <- some digitChar
  char ' '
  bag <- bagParser
  return (read quant :: Int, bag)

lineParser :: MParser BBag
lineParser = try $ do
  outerBag <- bagParser
  manyTill anyChar (string " contain ")
  bags <- (sepBy1 bagWithQuantParser (string ", ")) <|> emptyBagParser
  char '.'
  return (outerBag, bags)

fileParser :: MParser [BBag]
fileParser = try $ do
  bags <- sepEndBy1 lineParser (char '\n')
  return bags

-- create a map of bags to bags which contain them
processData :: [BBag] -> MBag
processData dat = Map.fromListWith
  (++)
  (concatMap
    (\(a, b) -> map (\(c, d) -> (d, [a]) ) b) 
    dat
  )

possibleContainers k m = f where
  v = Map.findWithDefault [] k m
  f = v ++ (foldl (\a b -> a ++ (possibleContainers b m) ) [] v)  

totalBags k m = f where
  v = Map.findWithDefault [] k m
  f = foldl (\a (b, c) -> a + b + b * (totalBags c m)) 0 v

solution inp = case parse fileParser "" inp of
  Left err -> putStr ("Error!")
  Right dat -> do
    print $ S.size $ S.fromList $ (possibleContainers ("shiny gold"::Bag) (processData dat))
    print $ totalBags "shiny gold" (Map.fromList dat)
    

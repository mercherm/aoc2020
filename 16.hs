import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Ix (inRange)
import Data.List
import Advent

type MParser = Parsec Void [Char]
type Range = (Int, Int)
type Rule = (String, [Range])
type Ticket = [Int]

main = do
  inp <- getContents
  solution inp

rangePsr :: MParser Range 
rangePsr = try $ do
  min <- some digitChar
  char '-'
  max <- some digitChar
  return (toInt min, toInt max)

rulePsr :: MParser Rule
rulePsr = try $ do
  label <- some (letterChar <|> (char ' '))
  string ": "
  ranges <- sepBy rangePsr (string " or ")
  return (label, ranges)

ticketPsr :: MParser Ticket
ticketPsr = try $ do
  nums <- sepBy (some digitChar) (char ',')
  return $ map toInt nums

filePsr :: MParser ([Rule], Ticket, [Ticket])
filePsr = try $ do
  rules <- sepEndBy rulePsr (char '\n')
  string "\nyour ticket:\n"
  ticket <- ticketPsr
  string "\n\nnearby tickets:\n"
  nearby <- sepEndBy ticketPsr (char '\n')
  return (rules, ticket, nearby)

checkValue rules v = any (\r -> inRange r v) rules
checkTicket rules ticket = all (checkValue rules) ticket
 
invalidValues rules ticket = filter
  (\v -> not $ checkValue rules v)
  ticket

checkRules :: [Rule] -> [[Int]] -> [(Int, [Rule])]
checkRules rs vs = map
      (\(vals, i) -> (i, filter
        (\(label, ranges) -> all 
          (\v -> any
            (\r -> inRange r v)
            ranges
          )
          vals
        )
        rs)
      ) 
      (zip vs [0..])

solve srt = foldl 
  (\o (i, ls) -> 
    (i, case find (\l -> notElem l (map snd o) ) ls of 
      Just x -> x
      Nothing -> ""
    ):o ) 
  [] 
  srt

solution inp = case parse filePsr "" inp of
  Right (rules, ticket, nearby) -> do  
    let flatRules = concatMap snd rules
    let errs = concatMap (invalidValues flatRules) nearby
    print $ sum errs
    -- init is ugly here, but iunno why parser is spitting empty list at end
    let validNearby = init $ filter (checkTicket flatRules) nearby
    let thingy = pivot validNearby
    let thongy = checkRules rules thingy
    let thangy = map (\(i, r) -> (i, map fst r) ) thongy
    let sorty = sortBy (\(_,a) (_,b) -> compare (length a) (length b)) thangy
    print $ product $ map 
      ((ticket!!).fst)
      (filter ((isPrefixOf "departure").snd) (solve sorty))

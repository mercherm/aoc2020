import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Bits
import Data.List

main = do
  inp <- getContents
  solution inp

type MParser = Parsec Void [Char]
type Instruction = (Int, [Char])
type Mem = (Int, Int)

mem :: MParser Instruction
mem = do
  _ <- string "mem"
  loc <- between (char '[') (char ']') (many digitChar)
  _ <- string " = "
  val <- many digitChar
  return (read loc :: Int, val)

mask :: MParser Instruction
mask = do
  _ <- string "mask = "
  val <- many (choice [digitChar, char 'X']) 
  return (0, val)

instructions :: MParser [Instruction]
instructions = (choice [mask, mem]) `sepEndBy` newline

replaceAt n v l = x ++ v:(tail xs) where
  (x, xs) = splitAt n l

permute mask = case findIndex (=='X') mask of
  Just i -> concatMap permute [replaceAt i 'A' mask, replaceAt i 'B' mask]
  Nothing -> [mask]
  
apMask :: [Char] -> Int -> Int
apMask mask val = foldl 
  (\v (m, i) -> 
    case m of
      '0' -> v
      '1' -> setBit v i
      'A' -> clearBit v i
      'B' -> setBit v i 
  ) 
  (val) 
  (zip (reverse mask) [0..])

getAddrs mask addr = map 
  (\msk -> apMask msk addr) 
  (permute mask)
  
updMem mem vals = foldl 
  (\mem (addr, val) -> case findIndex ((==addr).fst) mem of
    Just i -> replaceAt i (addr, val) mem 
    Nothing -> ((addr, val)):mem
  )
  mem
  vals

run :: [Instruction] -> ([Char], [Mem])
run ops =
  foldl (\(mask, mem) (addr, val) ->
    case addr of
      0 -> (val, mem)
      _ -> do
        let addrs = getAddrs mask addr
        let vals = take (length addrs) (repeat (read val :: Int)) 
        let nextMem = updMem mem (zip addrs vals) 
        (mask, nextMem)
    ) ("", []) ops  
  
solution inp = do
  case parse instructions "" inp of
    Right ops -> do 
      let (mask, mem) = run ops
      print $ sum (map (snd) mem)


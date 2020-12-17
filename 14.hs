import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import qualified Data.Vector as V
import Data.Bits

main = do
  inp <- getContents
  solution inp

type MParser = Parsec Void [Char]
type Instruction = (Int, [Char])


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
  val <- many (choice [digitChar, char 'X']) -- this could be more specific, but not necessary
  return (0, val)

instructions :: MParser [Instruction]
instructions = (choice [mask, mem]) `sepEndBy` newline

apMask :: [Char] -> [Char] -> Int
apMask mask val = foldl 
  (\v (c, i) -> 
    case c of
      'X' -> v
      '0' -> clearBit v i
      '1' -> setBit v i  
  ) 
  (read val :: Int) 
  (zip (reverse mask) [0..])

-- create a vector with size equal to 1 + the maximum memory address in ops
-- should probably be initMem size, but lazy.
initMem ops = V.replicate (1 + maximum (map (fst) ops)) 0

run :: [Instruction] -> ([Char], V.Vector Int)
run ops =
  foldl (\(mask, mem) (addr, val) ->
    case addr of
      0 -> (val, mem)
      _ -> (mask, mem V.// [(addr, apMask mask val)]) 
    ) ("", initMem ops) ops  
  
solution inp = do
  case parse instructions "" inp of
    Right ops -> do 
      let (mask, mem) = run ops
      print $ sum mem


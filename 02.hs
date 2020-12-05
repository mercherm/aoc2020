import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Data.Ix
import Data.Either 

type MParser = Parsec Void [Char]

-- look at that icky hack. someone doesn't know haskell, amirite?
main = do
  contents <- getContents
  print (solution (map (\ln -> fromRight (1,1,'a',"a") ln) (parseInp contents)))

lineParser :: MParser (Int, Int, Char, [Char])
lineParser = try $ do
  min <- some digitChar
  char '-'
  max <- some digitChar
  space
  ch <- letterChar
  char ':'
  space
  pw <- some letterChar
  return $ (read min :: Int, read max :: Int, ch, pw)

parseInp a = map (\ln -> runParser lineParser ln ln) (lines a)

cnt x = length . filter x 

checkPw :: (Int, Int, Char, [Char]) -> Bool
checkPw (min, max, ch, pw) = inRange (min, max) (cnt (==ch) pw)

checkPw2 :: (Int, Int, Char, [Char]) -> Bool
checkPw2 (pos1, pos2, ch, pw) = (pw!!(pos1-1) == ch) /= (pw!!(pos2-1) == ch) 

solution inp = [
    cnt checkPw inp,
    cnt checkPw2 inp
  ]

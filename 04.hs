import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Data.Char as C
import Data.Void
import qualified Data.List.Split as S
import Data.Either
import Data.List
import Data.Ix

type MParser = Parsec Void [Char]
type Field = ([Char], [Char])
type Passport = [Field]

main = do
  inp <- getContents
  print (solution inp)

fieldParser :: MParser Field
fieldParser = try $ do
  key <- some letterChar
  char ':'
  val <- some (alphaNumChar <|> char '#')
  return $ (key, val)

passportParser :: MParser Passport
passportParser = try $ do
  fields <- sepBy fieldParser (char ' ' <|> char '\n')
  return fields

fileParser inp = map (\p -> fromRight ([] :: Passport) (parse passportParser "" p)) (S.splitOn "\n\n" inp) 

reqFields = [
    "byr",
    "iyr",
    "eyr",
    "hgt",
    "hcl",
    "ecl",
    "pid"--,"cid"
  ]

isFieldValid (key, val) = 
  case key of  
    "byr" -> inRange (1920, 2002) (read val :: Int)
    "iyr" -> inRange (2010, 2020) (read val :: Int)
    "eyr" -> inRange (2020, 2030) (read val :: Int)
    "hgt" -> (\(ht, unt) -> 
               if unt == "cm" then inRange (150, 193) (read ht :: Int)
               else if unt == "in" then inRange (59, 76) (read ht :: Int)
               else False
             ) $ (splitAt ((length val) - 2) val)
    "hcl" -> (\(x:xs) -> length xs == 6 && all C.isHexDigit xs ) $ val
    "ecl" -> val `elem` ["amb","blu","brn","gry","grn","hzl","oth"]
    "pid" -> length val == 9 -- this might not be right (if there are ones with non-digits)
    _ -> False

validFields p = intersect
  reqFields 
  (map 
    (\(key, val) -> key) 
    (filter isFieldValid p) -- replace this with simply p for part one
  )

checkPassport p = length (validFields p) == length reqFields 

solution inp = length (filter (checkPassport) (fileParser inp))

module Main where
import Control.Monad
import Data.Char (chr, ord)
import Data.Bits (shift)
import Numeric (readDec, readHex, readOct)
import System.Environment
import Test.QuickCheck
import Text.ParserCombinators.Parsec hiding (spaces)

letters = ['a'..'z'] ++ ['A'..'Z']
symbols = "!$%&|*+-/:<=>?@^_~#"

symbol :: Parser Char
symbol = oneOf symbols

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
  deriving (Eq, Show)

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many parseStrStr
                 char '"'
                 return $ String $ concat x

parseStrStr :: Parser String
parseStrStr = escapedChar
          <|> normalChar

normalChar :: Parser String
normalChar = do
  c <- noneOf "\\\""
  return [c]

escapedChar :: Parser String
escapedChar = do
  char '\\'
  c <- oneOf escapables
  return [chr $ c2i c]

escapables = "0abtnvfr\"\\"

c2i '0'  =  0 -- NUL (null)
c2i 'a'  =  7 -- BEL (bell)
c2i 'b'  =  8 -- BS  (backspace)
c2i 't'  =  9 -- HT  (horizontal tab)
c2i 'n'  = 10 -- LF  (new line)
c2i 'v'  = 11 -- VT  (vertical tab)
c2i 'f'  = 12 -- FF  (form feed)
c2i 'r'  = 13 -- CR  (carriage return)
c2i '\"' = 34
c2i '\\' = 92

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = [first] ++ rest
               return $ case atom of
                          "#t" -> Bool True
                          "#f" -> Bool False
                          otherwise -> Atom atom

parseNumber :: Parser LispVal
parseNumber = liftM (Number . read) $ many1 digit
---- alt 1
-- parseNumber = many1 digit >>= \ds -> return $ (Number . read) ds
---- alt 2
-- parseNumber = do
--   ds <- many1 digit
--   return $ (Number . read) ds

parseRadixNumber :: Parser LispVal
parseRadixNumber = do
  char '#'
  radixPrefix <- oneOf "bodx"
  num <- many1 $ oneOf "0123456789abcdefABCDEF"
  return $ case radixPrefix of
    -- TODO: fix binary parsing
             -- TODO: find out how to jack in readBin
             -- 'b' -> readBin num
             'b' -> undefined --readBin num
             'o' -> Number $ readNum readOct num
             'd' -> Number $ readNum readDec num
             'x' -> Number $ readNum readHex num

readNum :: Num t1 => (String -> [(Integer, [Char])]) -> String -> Integer
readNum baseReader numStr = case baseReader numStr of
                              [(num, "")] -> num
                              otherwise -> 0 -- TODO: this should probably fail

readBin :: Parser LispVal
readBin = do
  ds <- many1 $ oneOf "01"
  let (int, _) = foldr (\d (sum, e) -> (sum + (ord d - 48) * e, shift e 1)) (0, 1) ds
  return $ Number $ toInteger int

parseExpr :: Parser LispVal
parseExpr = try parseRadixNumber
        <|> parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
                   Left err -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do args <- getArgs
          putStrLn (readExpr (args !! 0))


-- quickcheck --


-- from https://wiki.haskell.org/QuickCheck_as_a_test_set_generator
neStringOf :: [a] -> [a] -> Gen [a]
neStringOf charsStart charsRest =
  do s <- elements charsStart
     r <- listOf' $ elements charsRest
     return (s:r)

listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (0, n)
     vectorOf' k gen

vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]

instance Arbitrary LispVal where
  arbitrary = liftM Atom atomString

--    n <- choose $ oneof "abc"
--    return  $ Atom n

atomString :: Gen String
atomString = neStringOf letters $ letters ++ symbols

prop_atom :: Property
prop_atom = forAll atomString (\s -> parse parseAtom "atom" s == Right (Atom s))

-- TODO: expand to include escaped characters
stringString :: Gen String
stringString = listOf $ elements $ letters ++ symbols

prop_string :: Property
prop_string = forAll stringString (\s -> parse parseString "atom" ("\"" ++ s ++ "\"") == Right (String s))

--prop_number n = parse parseNumber "number" n == Right (Number n)

-- quickCheck (prop_atom :: String -> Bool)
-- generate arbitrary :: IO LispVal
-- sample atomString

import Control.Monad
import LispError
import LispEval
import LispVal
import LispParser
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC
import Text.Parsec

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

pAtom     = parse parseAtom         "atom err"
pChr      = parse parseChar         "character err"
pStr      = parse parseString       "string err"
pList     = parse parseList         "list err"
pPair     = parse parseDottedList   "pair err"
pNum      = parse parseNumber       "number err"
pRadixNum = parse parseRadixNumber  "radix number err"
pExpr     = parse parseExpr         "expression err"

showExpr :: String -> String
showExpr s = case pExpr s of
               Left err -> show err
               Right val -> show val

evalExpr :: String -> Either LispError LispVal
evalExpr e = readExpr e >>= eval

unitTests = testGroup "Unit tests"
  [ testCase "Simple atom is parsed" $
    (Right (Atom "foo")) @=? (pAtom "foo")


  , testCase "Character '#\\a' is parsed" $
    (Right (Character 'a')) @=? (pChr "#\\a ")
  , testCase "Character '#\\A' is parsed" $
    (Right (Character 'A')) @=? (pChr "#\\A ")
  , testCase "Character '#\\(' is parsed" $
    (Right (Character '(')) @=? (pChr "#\\( ")
  , testCase "Character '#\\ ' is parsed" $
    (Right (Character ' ')) @=? (pChr "#\\  ")
  -- TODO: fix parsing of these
  -- , testCase "Character '#\\space' is parsed" $
  --   (Right (Character ' ')) @=? (pChr "#\\space")
  -- , testCase "Character '#\\newline' is parsed" $
  --   (Right (Character '\n')) @=? (pChr "#\\newline")

  , testCase "Boolean true is parsed" $
    (Right (Bool True)) @=? (pAtom "#t")
  , testCase "Boolean false is parsed" $
    (Right (Bool False)) @=? (pAtom "#f")

  , testCase "Simple string is parsed" $
    (Right (String "str")) @=? (pStr "\"str\"")
  , testCase "String with escaped quote is parsed" $
    (Right (String "st\"r")) @=? (pStr "\"st\\\"r\"")


  , testCase "Number is parsed" $
    (Right (Number 1337)) @=? (pNum "1337")
  , testCase "Base 16 number is parsed" $
    (Right (Number 255)) @=? (pRadixNum "#xff")

  , testCase "List (no parens) is parsed" $
    (Right (List [Atom "f", Atom "x"])) @=? (pList "f x")
  , testCase "List (parens) is parsed" $
    (Right (List [Atom "f", Atom "x"])) @=? (pExpr "(f x)")

  , testCase "Dotted list is parsed" $
    (Right (DottedList [Atom "id"] $ Number 7)) @=? (pPair "id . 7")

  , testCase "Expression is parsed" $
    (Right (List [Atom "e", Number 2, Atom "x"])) @=? (pExpr "(e 2 x)")

  , testCase "Y-combinator is parsed" $
    (Right (List [Atom "define", Atom "Y",
                  List [Atom "lambda", List[Atom "le"],
                        List [List [Atom "lambda", List [Atom "f"], List [Atom "f", Atom "f"]],
                              List [Atom "lambda", List [Atom "f"],
                                    List [Atom "le", List [Atom "lambda", List [Atom "x"],
                                                           List [List [Atom "f", Atom "f"], Atom "x"]]]]]]]))
    @=?
    (pExpr $
      "(define Y                \
      \  (lambda (le)           \
      \    ((lambda (f) (f f))  \
      \     (lambda (f)         \
      \       (le (lambda (x)   \
      \             ((f f) x)))))))")

  , testCase "Expression is parsed and showed" $
    "(quote (1 3 (\"this\" \"one\")))" @=? (showExpr "'(1 3 (\"this\" \"one\"))")

  , testCase "String equality" $
    (Right (Bool True)) @=? (evalExpr "(string=? \"test\"  \"test\")")
  , testCase "Less than is true" $
    (Right (Bool True)) @=? (evalExpr "(< 2 3)")
  , testCase "Greater than is true" $
    (Right (Bool True)) @=? (evalExpr "(> 3 2)")
  , testCase "Greater or equal than is true" $
    (Right (Bool True)) @=? (evalExpr "(>= 3 3)")
  , testCase "String less than is true" $
    (Right (Bool True)) @=? (evalExpr "(string<? \"abc\" \"bba\")")

  , testCase "'if' false branch" $
    (Right (String "yes")) @=? (evalExpr "(if (> 2 3) \"no\" \"yes\")")
  , testCase "'if' true branch" $
    (Right (Number 9)) @=? (evalExpr "(if (= 3 3) (+ 2 3 (- 5 1)) \"unequal\")")

  , testCase "'car' returns first element of list" $
    (Right (Atom "a")) @=? (evalExpr "(car '(a b c))")
  , testCase "'car' returns element from singleton list" $
    (Right (Atom "a")) @=? (evalExpr "(car '(a))")
  , testCase "'car' returns first element from dotted list" $
    (Right (Atom "a")) @=? (evalExpr "(car '(a b . c))")
  , testCase "'car' throws an error for non list argument" $
    (Left (TypeMismatch "pair" (Atom "a"))) @=? (evalExpr "(car 'a)")
  , testCase "'car' throws error for more that one argument" $
    (Left (NumArgs 1 [Atom "a", Atom "b"])) @=? (evalExpr "(car 'a 'b)")

  , testCase "'cdr' returns rest of list" $
    (Right (List [Atom "b", Atom "c"])) @=? (evalExpr "(cdr '(a b c))")
  , testCase "'cdr' returns singleton list with last element from list of two elements" $
    (Right (List [Atom "b"])) @=? (evalExpr "(cdr '(a b))")
  , testCase "'cdr' returns NIL from singleton list" $
    (Right (List [])) @=? (evalExpr "(cdr '(a))")
  , testCase "'cdr' returns second element of pair" $
    (Right (Atom "b")) @=? (evalExpr "(cdr '(a . b))")
  , testCase "'cdr' returns dotted list from dotted list" $
    (Right (DottedList [Atom "b"] (Atom "c"))) @=? (evalExpr "(cdr '(a b . c))")
  , testCase "'cdr' throws an error for non list argument" $
    (Left (TypeMismatch "pair" (Atom "a"))) @=? (evalExpr "(cdr 'a)") -- = error – not a list
  , testCase "'cdr' throws error for more that one argument " $
    (Left (NumArgs 1 [Atom "a", Atom "b"])) @=? (evalExpr "(cdr 'a 'b)") -- = error – too many arguments

  , testCase "'cons' builds a pair" $
    (Right (DottedList [List [Atom "this", Atom "is"]] $ Atom "test")) @=? (evalExpr "(cons '(this is) 'test)")
  , testCase "'cons' builds a list" $
    (Right (List [List [Atom "this", Atom "is"]])) @=? (evalExpr "(cons '(this is) '())")

  , testCase "'eqv?' returns #f for unequal numbers" $
    (Right (Bool False)) @=? (evalExpr "(eqv? 1 3)")
  , testCase "'eqv?' returns #t for equal numbers" $
    (Right (Bool True)) @=? (evalExpr "(eqv? 3 3)")
  , testCase "'eqv?' returns #t for equal atoms" $
    (Right (Bool True)) @=? (evalExpr "(eqv? 'atom 'atom)")
  , testCase "'eqv?' returns #f for different types" $
    (Right (Bool False)) @=? (evalExpr "(eqv? 2 \"2\")")
  , testCase "'equal?' returns #t for different types" $
    (Right (Bool True)) @=? (evalExpr "(equal? 2 \"2\")")
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "An atom is parsed" $
    forAll atomString (\s -> pAtom s == Right (Atom s))
  , QC.testProperty "A string is parsed" $
    forAll stringString (\s -> pStr ("\"" ++ s ++ "\"") == Right (String s))
  ]

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

-- TODO: expand to include escaped characters
stringString :: Gen String
stringString = listOf $ elements $ letters ++ symbols

--prop_number n = parse parseNumber "number" n == Right (Number n)

-- quickCheck (prop_atom :: String -> Bool)
-- generate arbitrary :: IO LispVal
-- sample atomString

import Control.Monad
import Parser
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


pAtom = parse parseAtom "atom err"
pStr = parse parseString "string err"
--pElemSep = parse elemSep "elemSep err"
--pEmptyList = parse emptyList "emptyList err"
--pInt = parse int "int err"
--pTuple = parse tuple "tuple err"

unitTests = testGroup "Unit tests"
  [ testCase "Simple atom is parsed" $
    (Right (Atom "foo")) @=? (pAtom "foo")

  , testCase "Simple string is parsed" $
    (Right (String "str")) @=? (pStr "\"str\"")

  -- , testCase "Empty list is parsed" $
  --   (Right Nil) @=? (pEmptyList "[]")

  -- , testCase "Simple integer is parsed" $
  --   (Right (Integer 123)) @=? (pInt "123")

  -- , testCase "Empty tuple is parsed" $
  --   (Right (Tuple [])) @=? (parse (tuple [Nil]) "tuple err" "{}")

    -- , testCase "Tuple with atom is parsed" $
  --   (Right (Tuple [Atom "foo"]) @=? ((tuple atom) "{foo}")
  ]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "An atom is parsed" $
    forAll atomString (\s -> parse parseAtom "atom" s == Right (Atom s))
  , QC.testProperty "A string is parsed" $
    forAll stringString (\s -> parse parseString "atom" ("\"" ++ s ++ "\"") == Right (String s))
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

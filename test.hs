import Control.Applicative
import qualified Control.Exception as C
import Language.Nock5K
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.HUnit
import Test.QuickCheck
import Text.ParserCombinators.Parsec (parse)
import Text.Printf

main = defaultMain tests

instance Arbitrary Noun where
  arbitrary = choose (0, 32) >>= arbD
    where
      arbD :: Int -> Gen Noun
      arbD 0 = (Atom . abs) <$> arbitrary
      arbD n = do coin <- arbitrary
                  if coin
                    then (Atom . abs) <$> arbitrary
                    else (:-) <$> arbD (n - 1) <*> arbD (n - 1)

parsenoun n = case parse noun "" n of
  Left e -> error "parse"
  Right n -> n

prop_parse_show n = n == (parsenoun . show) n

prop_dec a' = nock (Atom (a + 1) :- dec) == Atom a
  where
    ds = "[8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]"
    dec = parsenoun ds
    a = abs a'

prop_6_is_if a' b = nock (ifs $ Atom 0) == Atom (a + 1) && nock (ifs $ Atom 1) == b
  where
    ifs c = Atom a :- Atom 6 :- (Atom 1 :- c) :- (Atom 4 :- Atom 0 :- Atom 1) :- (Atom 1 :- b)
    a = abs a'

test_hint_crash =
  C.catch (nock bad `seq` assertFailure "Hint not evaluated")
          (\e -> do let se = show (e :: C.ErrorCall)
                    if se == "/a"
                      then return ()
                      else assertFailure $ "Exception: " ++ se)
 where
  bad = Atom 42 :- Atom 10 :- (Atom 0 :- Atom 0 :- Atom 2) :- Atom 0 :- Atom 1

tests = [ testProperty "parse.show"    prop_parse_show
        , testProperty "decrement"     prop_dec
        , testProperty "6_is_if"       prop_6_is_if
        , testCase     "10_hint_crash" test_hint_crash
        ]

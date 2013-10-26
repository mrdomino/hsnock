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

instance Arbitrary Noun where
  arbitrary = choose (0, 32) >>= arbD
    where
      arbD :: Int -> Gen Noun
      arbD 0 = (Atom . abs) <$> arbitrary
      arbD n = do coin <- arbitrary
                  if coin
                    then arbD 0
                    else (:-) <$> arbD (n - 1) <*> arbD (n - 1)

pn n = case parse noun "" n of Right n -> n

prop_parse_show n = n == (pn . show) n

prop_dec a' = nock (Atom (a + 1) :- dec) == (Right $ Atom a)
  where
    ds = "[8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]"
    dec = pn ds
    a = abs a'

prop_6_is_if a' b = nock (ifs $ Atom 0) == Right (Atom (a + 1)) && nock (ifs $ Atom 1) == Right b
  where
    ifs c = Atom a :- Atom 6 :- (Atom 1 :- c) :- (Atom 4 :- Atom 0 :- Atom 1) :- (Atom 1 :- b)
    a = abs a'

test_hint_crash = assert $ nock bad == Left "/a"
  where bad = pn "[42 10 [0 0 2] 0 1]"

test_eval_strict = assert $ nock bad == Left "/a"
  where bad = pn "[0 2 [0 2] 1 1 42]"

tests = [ testProperty "parse.show"    prop_parse_show
        , testProperty "decrement"     prop_dec
        , testProperty "6_is_if"       prop_6_is_if
        , testCase     "10_hint_crash" test_hint_crash
        , testCase     "eval_strict"   test_eval_strict
        ]

main = defaultMain tests

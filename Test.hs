import Control.Monad
import Nock5K.Parse
import Nock5K.Spec
import Test.QuickCheck
import Text.ParserCombinators.Parsec (parse)
import Text.Printf

main = mapM_ (\(s,a) -> printf "%-25s: " s >> a) tests

instance Arbitrary Noun where
  arbitrary = do coin <- arbitrary
                 if coin
                 then liftM (Atom . abs) arbitrary
                 else do a <- arbitrary
                         b <- arbitrary
                         return $ a :- b

prop_parse_show n = n == ps n
  where ps n = case parse noun "" (show n) of
                 Left e -> error "parse"
                 Right n -> n

prop_dec a' = nock (Atom (a + 1) :- dec) == Atom a
  where
    ds = "[8 [1 0] 8 [1 6 [5 [0 7] 4 0 6] [0 6] 9 2 [0 2] [4 0 6] 0 7] 9 2 0 1]"
    dec = case parse noun "" ds of
            Left e -> error "parse"
            Right n -> n
    a = abs a'

prop_6_is_if a' b' = nock (ifs $ Atom 0) == Atom (a + 1) && nock (ifs $ Atom 1) == Atom b
  where
    ifs c = (Atom a :- Atom 6 :- (Atom 1 :- c) :- (Atom 4 :- Atom 0 :- Atom 1) :- (Atom 1 :- Atom b))
    a = abs a'
    b = abs b'

tests = [("parse_show", quickCheck prop_parse_show)
        ,("decrement", quickCheck prop_dec)
        ,("6_is_if", quickCheck prop_6_is_if)]

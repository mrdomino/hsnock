module Language.Nock5K.Parse where

import Control.Applicative ((<$>))
import Language.Nock5K.Spec
import Text.ParserCombinators.Parsec

instance Show Noun where
  show (Atom a) = show a
  show x@(_ :- _) = "[" ++ showCell x ++ "]"
    where showCell (a :- b) = show a ++ " " ++ showCell b
          showCell a = show a

noun :: Parser Noun
noun = atom <|> cell

atom :: Parser Noun
atom = (Atom . read) <$> many1 digit

cell :: Parser Noun
cell = foldr1 (:-) <$> (noun `sepBy2` char ' ') `surroundBy` "[]"
  where
    p `sepBy2` sep = do { a <- p; sep; bs <- p `sepBy1` sep; return (a:bs) }
    p `surroundBy` (s:e:_) = do { char s; a <- p; char e; return a }

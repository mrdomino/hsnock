module Nock5K.Parse (noun) where

import Nock5K.Spec
import Text.ParserCombinators.Parsec

instance Show Noun where
  show (Atom a) = show a
  show x@(_ :- _) = "[" ++ showCell x ++ "]"
    where showCell (a :- b) = show a ++ " " ++ showCell b
          showCell a = show a

noun :: Parser Noun
noun = atom <|> cell

atom :: Parser Noun
atom = many1 digit >>= (return . Atom . read)

cell :: Parser Noun
cell = do
  char '['
  a <- noun
  char ' '
  bs <- noun `sepBy1` char ' '
  char ']'
  return $ foldr1 (:-) (a:bs)

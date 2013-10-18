#!/usr/bin/env runhaskell

import Nock5K

import System.IO (hPrint, stderr)
import Text.ParserCombinators.Parsec (
  (<|>), Parser, char, digit, many1, oneOf, optionMaybe, parse, sepBy1)

instance Show Noun where
  show (Atom a) = show a
  show x@(Cell _ _) = "[" ++ showCell x ++ "]"
    where showCell (Cell a b) = show a ++ " " ++ showCell b
          showCell a = show a

doop :: Char -> Noun -> Noun
doop '?' = wut
doop '+' = lus
doop '=' = tis
doop '/' = fas
doop '*' = tar
doop _ = error "op"

stmt :: Parser (Maybe Char, Noun)
stmt = do
  o <- optionMaybe $ oneOf "?+=/*"
  n <- noun
  return (o, n)

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
  return $ foldr1 Cell (a:bs)

main :: IO ()
main = do
  ln <- getLine
  case (parse stmt "" ln) of
    Left pe -> hPrint stderr pe
    Right (o,n) -> case o of
      Nothing -> print n
      Just op -> print $ doop op n

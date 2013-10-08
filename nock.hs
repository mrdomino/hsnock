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

stmt :: Parser Noun
stmt = do
  op <- optionMaybe $ oneOf "?+=/*"
  nn <- noun
  return $ case op of
    Nothing -> nn
    Just o -> doop o nn

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
    Right n -> print n

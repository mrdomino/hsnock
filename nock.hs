#!/usr/bin/env runhaskell

import Prelude hiding (catch)

import Nock5K.Parse
import Nock5K.Spec

import Control.Exception (SomeException, catch)
import Control.Monad (forever)
import System.IO (hFlush, hPrint, stderr, stdout)
import Text.ParserCombinators.Parsec (parse)

repl :: IO ()
repl = forever rep
 where
  rep = do putStr "nock "
           hFlush stdout
           ln <- getLine
           case parse noun "stdin" ln of
             Left pe -> hPrint stderr pe
             Right n -> ep n
  ep n = catch ((print . nock) n)
               (\ioe -> hPrint stderr (ioe :: SomeException) >> repl)

main :: IO ()
main = repl

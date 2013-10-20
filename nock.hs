#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}
import Prelude hiding (catch)

import Nock5K.Parse
import Nock5K.Spec
import Control.Exception
import System.Console.Readline
import Text.ParserCombinators.Parsec (parse)

repl = do ln <- readline "nock "
          case ln of
            Nothing     -> return ()
            Just "exit" -> return ()
            Just s -> do addHistory s
                         case parse noun "" s of
                           Left e -> print e
                           Right n -> ep n
                         repl
  where
    ep n = (print . nock) n `catch` (\(e :: SomeException) -> print e)

main = repl

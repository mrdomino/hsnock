#!/usr/bin/env runhaskell
{-# LANGUAGE ScopedTypeVariables #-}

import Nock5K.Parse
import Nock5K.Spec
import Control.Exception
import System.Exit
import System.IO
import System.IO.Error (isEOFError)
import Text.ParserCombinators.Parsec (parse)

rep = srpa >>= print . nock
  where
    srpa = s >> r >>= pa
    s = putStr "nock " >> hFlush stdout
    r = getLine
    pa l = case parse noun "" l of
      Left e -> hPrint stderr e >> srpa
      Right n -> return n

repl = rep `catches` hs >> repl
  where
    hs = [ Handler (\(e :: IOException) ->
             if isEOFError e then exitSuccess else hPrint stderr e)
         , Handler (\(e :: SomeException) -> hPrint stderr e) ]

desc = "Nock 5K. ^D to exit."
main = putStrLn desc >> repl

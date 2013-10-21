module Language.Nock5K.Repl where

import Language.Nock5K.Parse
import Language.Nock5K.Spec
import qualified Control.Exception as C
import System.Console.Readline
import Text.ParserCombinators.Parsec

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
    ep n = (print . nock) n `C.catch` (\e -> print (e :: C.SomeException))

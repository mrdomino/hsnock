module Language.Nock5K.Repl where

import Language.Nock5K.Parse
import Language.Nock5K.Spec
import System.Console.Readline
import System.IO
import Text.ParserCombinators.Parsec

repl = do ln <- readline "nock "
          case ln of
            Nothing     -> return ()
            Just "exit" -> return ()
            Just s -> do addHistory s
                         case parse noun "" s of
                           Left e -> (sep . show) e
                           Right n -> ep n
                         repl
  where ep n = case nock n of
           Left e -> sep e
           Right n -> print n
        sep e = hPutStrLn stderr $ "error: " ++ e

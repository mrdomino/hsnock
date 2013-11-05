-- | Implementation of the Nock 5K spec.
module Language.Nock5K (
    -- * Structures
    Noun(..),
    Nock,
    -- * Reduction
    nock,
    -- * Parser
    noun,
    -- * REPL
    repl
  ) where

import Language.Nock5K.Parse
import Language.Nock5K.Repl
import Language.Nock5K.Spec

-- | G-machine code execution
module Hschain.Utxo.Lang.Core.Gmachine.Monad(
   Error(..)
) where

import Control.DeepSeq
import GHC.Generics (Generic)

import Hschain.Utxo.Lang.Core.Data.Prim

-- | Errors of execution
data Error
  = BadType             -- ^ encountered illegal application
  | BadAddr Addr        -- ^ address is not defined on the heap
  | NotFound Name       -- ^ can not find global name
  | StackIsEmpty        -- ^ Need to read element from stack but it is empty
  | VstackIsEmpty       -- ^ Need to read element from Vstack but it is empty
  | DumpIsEmpty         -- ^ Attempt to read empty dump
  | MissingCase         -- ^ missing case-alternative
  | FailedToDeserialise -- ^ Failed todeserialise term
  | BottomTerm          -- ^ Bottom termination
  deriving stock    (Show, Eq, Generic)
  deriving anyclass (NFData)

-- |
module TM.Core.Common (env) where

import HSChain.Crypto (hashBlob)
import Hschain.Utxo.Lang.Types (InputEnv(..),Box(..),BoxId(..),Script(..))


env :: InputEnv
env = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = Box
    { box'id     = BoxId $ hashBlob ""
    , box'value  = 100
    , box'script = Script ""
    , box'args   = mempty
    }
  , inputEnv'inputs   = mempty
  , inputEnv'outputs  = mempty
  , inputEnv'args     = mempty
  }

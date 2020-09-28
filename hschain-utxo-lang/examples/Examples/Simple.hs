{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- SKI calculus examples
module Examples.Simple where

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Types
import Examples.SKI (mkMain)

-- | Addition of two integers
progAddition :: CoreProg
progAddition = CoreProg
  [ mkMain $ Typed
    { typed'value = EAp
                    (EAp (EPrimOp OpAdd) (EPrim (PrimInt 1)))
                    (EPrim (PrimInt 100))
    , typed'type  = IntT
    }
  ]

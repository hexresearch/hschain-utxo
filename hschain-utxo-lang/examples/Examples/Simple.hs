{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- SKI calculus examples
module Examples.Simple where

import Hschain.Utxo.Lang.Core.Compile
import Hschain.Utxo.Lang.Core.Data.Prim
import Examples.SKI (mkMain)

-- | Addition of two integers
progAddition :: CoreProg
progAddition = CoreProg
  [ mkMain $ Typed
    { typed'value = EAp
                    (EAp (EVar (Typed "+" (funT [intT,intT] intT))) (EPrim (PrimInt 1)))
                    (EPrim (PrimInt 100))
    , typed'type  = intT
    }
  ]

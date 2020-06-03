module Hschain.Utxo.Lang.Compile.LambdaLifting(
  lambdaLifting
) where

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.LambdaLifting.Abstract
import Hschain.Utxo.Lang.Compile.LambdaLifting.Collect
import Hschain.Utxo.Lang.Compile.LambdaLifting.FreeVars
import Hschain.Utxo.Lang.Compile.LambdaLifting.Rename

lambdaLifting :: CoreProg -> CoreProg
lambdaLifting = collect . rename . abstract . annotateFreeVars


-- | Functions to work with dump
module Hschain.Utxo.Lang.Core.Gmachine.Eval.Dump(
    loadDump
  , saveDump
) where

import Hschain.Utxo.Lang.Core.Gmachine.Monad
import Hschain.Utxo.Lang.Core.Data.Code (Code)
import Hschain.Utxo.Lang.Core.Data.Stack (Stack)

import qualified Hschain.Utxo.Lang.Core.Data.Dump as Dump

loadDump :: Exec ()
loadDump = do
  (code, stack) <- popDump
  putCode code
  putStack stack

saveDump :: Exec ()
saveDump = do
  code <- getCode
  stack <- getStack
  insertDump code stack

popDump :: Exec (Code, Stack)
popDump = do
  dump <- getDump
  let (mRes, dump') = Dump.pop dump
  putDump dump'
  maybe dumpIsEmpty pure mRes

insertDump :: Code -> Stack -> Exec ()
insertDump code stack = modifyDump $ Dump.put code stack


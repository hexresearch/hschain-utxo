-- | Helpers to work with Vstack (stack of primitive values).
module Hschain.Utxo.Lang.Core.Gmachine.Eval.Vstack(
    mkPrim
  , getExpr
  , updatePrim
  , pushBasic
  , popInt
  , putInt
  , popText
  , putText
  , popBS
  , putBS
  , popBool
  , putBool
  , popSigma
  , putSigma
  , popVstack
  , putVstack
) where

import Data.Int
import Data.ByteString (ByteString)
import Data.Text (Text)

import Hschain.Utxo.Lang.Sigma

import Hschain.Utxo.Lang.Core.Data.Node
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine.Monad
import Hschain.Utxo.Lang.Core.Gmachine.Eval.Heap
import Hschain.Utxo.Lang.Core.Gmachine.Eval.Stack

import qualified Hschain.Utxo.Lang.Core.Data.Heap as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Vstack as Vstack

pushBasic :: Prim -> Exec ()
pushBasic n = putVstack n

-- | shortcuts for sequence [MkPrim, Update n]
updatePrim :: Int -> Exec ()
updatePrim n = do
  topAddr <- alloc . NodePrim =<< popVstack
  nAddr <- lookupAddr n
  modifyHeap $ Heap.insertNode nAddr (NodeInd topAddr)

mkPrim :: Exec ()
mkPrim = do
  addr <- alloc . NodePrim =<< popVstack
  putAddr addr

-- | Implementation of Get instruction
getExpr :: Exec ()
getExpr = do
  node <- lookupHeap =<< popAddr
  maybe badType putVstack $ getNodePrim node

popInt :: Exec Int64
popInt = popVstackBy getPrimInt

putInt :: Int64 -> Exec ()
putInt = putVstackBy PrimInt

popText :: Exec Text
popText = popVstackBy getPrimText

putText :: Text -> Exec ()
putText = putVstackBy PrimText

popBS :: Exec ByteString
popBS = popVstackBy getPrimBytes

putBS :: ByteString -> Exec ()
putBS = putVstackBy PrimBytes

popBool :: Exec Bool
popBool = popVstackBy getPrimBool

putBool :: Bool-> Exec ()
putBool = putVstackBy PrimBool

popSigma :: Exec (Sigma PublicKey)
popSigma = popVstackBy getPrimSigma

putSigma :: Sigma PublicKey -> Exec ()
putSigma = putVstackBy PrimSigma

popVstack :: Exec Prim
popVstack = fromError VstackIsEmpty $ stateVstack Vstack.pop

putVstack :: Prim -> Exec ()
putVstack n = modifyVstack (Vstack.put n)

popVstackBy :: (Prim -> Maybe a) -> Exec a
popVstackBy extract = do
  fromError BadType $ fmap extract popVstack

putVstackBy :: (a -> Prim) -> a -> Exec ()
putVstackBy wrap = putVstack . wrap



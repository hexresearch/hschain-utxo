-- | Helpers to work with Heap.
module Hschain.Utxo.Lang.Core.Gmachine.Eval.Heap(
    alloc
  , lookupHeap
) where

import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Data.Node(Node)
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Gmachine.Monad

import qualified Hschain.Utxo.Lang.Core.Data.Heap as Heap

-- heap

-- | allocate new cell for the node on the heap,
alloc :: Node -> Exec Addr
alloc node = stateHeap (Heap.alloc node)

-- | Get the node stored in the heap by its address.
lookupHeap :: Addr -> Exec Node
lookupHeap addr = fromError (BadAddr addr) $
  fmap (Heap.lookup addr) getHeap



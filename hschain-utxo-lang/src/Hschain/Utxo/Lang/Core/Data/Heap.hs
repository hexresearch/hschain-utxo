-- | Module defines type for and function to work with heap
module Hschain.Utxo.Lang.Core.Data.Heap(
    Node(..)
  , Heap
  , empty
  , alloc
  , lookup
  -- * Globals
  , Globals
  , initGlobals
  , lookupGlobal
) where

import Prelude hiding (lookup)

import Data.IntMap (IntMap)
import Data.Map.Strict (Map)

import Hschain.Utxo.Lang.Core.Data.Code
import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M

-- | Heap of the program
data Heap = Heap
  { heap'values    :: IntMap Node   -- ^ Values stored on the heap
  , heap'freshAddr :: Addr          -- ^ fresh addresses counter
  }

-- | Memory to store global definitions
newtype Globals = Globals (Map Name Addr)

-- | Lookup the address of the global definition
lookupGlobal :: Name -> Globals -> Maybe Addr
lookupGlobal name (Globals m) = M.lookup name m

initGlobals :: [(Name, Addr)] -> Globals
initGlobals = Globals . M.fromList

data Node
  = NodeInt !Int         -- ^ constant integer
  | NodeCons !Addr !Addr -- ^ cons-constructor
  | Ap !Addr !Addr       -- ^ application
  | Fun !Int !Code       -- ^ supercombinator with k-arguments and code instructions
  | Hole                 -- ^ placeholder node to be filled

-- | Create empty heap
empty :: Heap
empty = Heap
  { heap'values    = mempty
  , heap'freshAddr = 0
  }

alloc :: Node -> Heap -> (Addr, Heap)
alloc node heap = (addr, insertNode addr node heap')
  where
    (addr, heap') = getFreshAddr heap

lookup :: Addr -> Heap -> Maybe Node
lookup addr heap = IM.lookup addr $ heap'values heap

insertNode :: Addr -> Node -> Heap -> Heap
insertNode addr node heap = heap
  { heap'values = IM.insert addr node $ heap'values heap }

getFreshAddr :: Heap -> (Addr, Heap)
getFreshAddr = undefined


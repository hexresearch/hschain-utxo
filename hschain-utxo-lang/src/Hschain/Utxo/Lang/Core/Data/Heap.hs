-- | Module defines type for and function to work with heap
module Hschain.Utxo.Lang.Core.Data.Heap(
    Node(..)
  , Heap
  , empty
  , alloc
  , lookup
  , insertNode
  -- * Globals
  , Globals
  , initGlobals
  , lookupGlobalScomb
  , lookupGlobalConst
  , insertGlobalConst
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
  } deriving (Show, Eq)

-- | Memory to store global definitions
data Globals = Globals
  { globals'scombs :: Map Name Addr
  , globals'consts :: Map Int  Addr
  } deriving (Show, Eq)

-- | Lookup the address of the global definition
lookupGlobalScomb :: Name -> Globals -> Maybe Addr
lookupGlobalScomb name gs = M.lookup name (globals'scombs gs)

lookupGlobalConst :: Int -> Globals -> Maybe Addr
lookupGlobalConst name gs = M.lookup name (globals'consts gs)

insertGlobalConst :: Int -> Addr -> Globals -> Globals
insertGlobalConst val addr gs =
  gs { globals'consts = M.insert val addr $ globals'consts gs }

initGlobals :: [(Name, Addr)] -> Globals
initGlobals = (\combs -> Globals combs M.empty) . M.fromList

data Node
  = NodeInt !Int         -- ^ constant integer
  | NodeCons !Addr !Addr -- ^ cons-constructor
  | NodeInd !Addr        -- ^ indirection node
  | Ap !Addr !Addr       -- ^ application
  | Fun !Int !Code       -- ^ supercombinator with k-arguments and code instructions
  | Hole                 -- ^ placeholder node to be filled
  deriving (Show, Eq)

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
getFreshAddr h = (freshAddr, heap')
  where
    freshAddr = heap'freshAddr h
    heap' = h { heap'freshAddr = freshAddr + 1 }



-- | Module defines type for and function to work with heap
module Hschain.Utxo.Lang.Core.Data.Heap(
    Heap
  , empty
  , alloc
  , lookup
  , insertNode
  -- * Globals
  , Globals
  , initGlobals
  , lookupGlobalScomb
  , insertGlobalScomb
  , lookupGlobalConst
  , insertGlobalConst
) where

import Prelude hiding (lookup)

import Data.IntMap (IntMap)
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Data.Code
import Hschain.Utxo.Lang.Core.Data.Node
import Hschain.Utxo.Lang.Core.Data.Prim

import qualified Data.IntMap as IM
import qualified Data.Map.Strict as M

-- | Heap of the program
data Heap = Heap
  { heap'values    :: IntMap Node   -- ^ Values stored on the heap
  , heap'freshAddr :: Addr          -- ^ fresh addresses counter
  } deriving (Show, Eq)

-- | Memory to store global definitions
data Globals = Globals
  { globals'scombs      :: Map GlobalName Addr
  , globals'constPrims  :: Map Prim       Addr
  , globals'constInts   :: IntMap         Addr
  } deriving (Show, Eq)

-- | Lookup the address of the global definition
lookupGlobalScomb :: GlobalName -> Globals -> Maybe Addr
lookupGlobalScomb name gs = M.lookup name (globals'scombs gs)

insertGlobalScomb :: GlobalName -> Addr -> Globals -> Globals
insertGlobalScomb name addr gs = gs
  { globals'scombs = M.insert name addr $ globals'scombs gs }

lookupGlobalConst :: Prim -> Globals -> Maybe Addr
lookupGlobalConst prim gs =
  case prim of
    PrimInt n -> IM.lookup n (globals'constInts gs)
    _         -> M.lookup prim (globals'constPrims gs)

insertGlobalConst :: Prim -> Addr -> Globals -> Globals
insertGlobalConst prim addr gs =
  case prim of
    PrimInt n -> gs { globals'constInts  = IM.insert n    addr $ globals'constInts gs }
    _         -> gs { globals'constPrims = M.insert  prim addr $ globals'constPrims gs }

initGlobals :: [(GlobalName, Addr)] -> Globals
initGlobals = (\combs -> Globals combs M.empty IM.empty) . M.fromList



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



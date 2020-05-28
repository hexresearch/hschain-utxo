-- | Heap nodes
module Hschain.Utxo.Lang.Core.Data.Node(
    Node(..)
  , getNodeApArg
  , getNodePrim
) where

import Data.Sequence(Seq)
import Data.Text(Text)

import Hschain.Utxo.Lang.Core.Data.Code
import Hschain.Utxo.Lang.Core.Data.Prim

data Node
  = NodePrim !Prim              -- ^ constant primitive
  | NodeInd !Addr               -- ^ indirection node
  | NodeConstr !Int (Seq Addr)  -- ^ constructor (integer-tag, addresses to arguments)
  | Ap !Addr !Addr              -- ^ application
  | Fun !Int !Code              -- ^ supercombinator with k-arguments and code instructions
  deriving (Show, Eq)


-- | Reads argument of application from the node
getNodeApArg :: Node -> Maybe Addr
getNodeApArg = \case
  Ap _ a -> Just a
  _      -> Nothing

-- | Reads primitive value from node
getNodePrim :: Node -> Maybe Prim
getNodePrim = \case
  NodePrim p -> Just p
  _          -> Nothing



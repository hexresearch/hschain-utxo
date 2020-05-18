-- | G-machine code execution
module Hschain.Utxo.Lang.Core.Gmachine(

) where

import Data.IntMap (IntMap)

import Data.Text (Text)
import Data.Sequence (Seq)

type Ident = Text

-- | Graph of the program
type Graph = IntMap (Node Int)

-- | Current stack of the G-machine
data Stack = Stack

-- | Code to feed to machine
data Code = Code

data Dump = Dump

data Node a
  = NodeInt !Int      -- ^ constant integer
  | NodeCons a a      -- ^ cons-constructor
  | Ap a a            -- ^ application
  | Fun !Int !Code    -- ^ supercombinator with k-arguments and code instructions
  | Hole              -- ^ placeholder node to be filled

-- | G-machine is FSM for fast graph reduction
data Gmachine = Gmachine
  { gmachine'graph :: Graph
  , gmachine'stack :: Stack
  , gmachine'code  :: Code
  , gmachine'dump  :: Dump
  }

step :: Gmachine -> Gmachine
step Gmachine{..} = undefined



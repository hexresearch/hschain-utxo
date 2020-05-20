module Hschain.Utxo.Lang.Core.Compile.Prog(
    CoreProg
  , Scomb(..)
  , Expr(..)
  , CompiledScomb(..)
  , compile
  , compileSc
) where

import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Gmachine
import Hschain.Utxo.Lang.Core.Compile.Env

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..))
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Globals, Node(..))
import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Data.List       as L

import qualified Hschain.Utxo.Lang.Core.Data.Code as Code
import qualified Hschain.Utxo.Lang.Core.Data.Heap as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Stat as Stat

type CoreProg = [Scomb]

-- | Supercobinators do not contain free variables.
--
-- > S a1 a2 a3 = expr
data Scomb = Scomb
  { scomb'name :: Name           -- ^ name
  , scomb'args :: Vector Name    -- ^ list of arguments
  , scomb'body :: Expr           -- ^ body
  }

data Expr
  = EVar !Name
  | ENum !Int
  | EAp  Expr Expr
  deriving (Show, Eq)

-- | Compiled supercombinator
data CompiledScomb = CompiledScomb
  { compiledScomb'name  :: Name   -- ^ name
  , compiledScomb'arity :: Int    -- ^ size of argument list
  , compiledScomb'code  :: Code   -- ^ code to instantiate combinator
  } deriving (Show, Eq)

compile :: CoreProg -> Gmachine
compile prog = Gmachine
  { gmachine'code    = Code.init
  , gmachine'stack   = mempty
  , gmachine'heap    = heap
  , gmachine'globals = globals
  , gmachine'stats   = Stat.empty
  }
  where
    (heap, globals) = buildInitHeap prog

buildInitHeap :: CoreProg -> (Heap, Globals)
buildInitHeap prog = (heap, Heap.initGlobals globalElems)
  where
    compiled = fmap compileSc prog

    (heap, globalElems) = L.foldl' allocateSc (Heap.empty, []) compiled

    allocateSc (heap, globals) CompiledScomb{..} = (heap', (compiledScomb'name, addr) : globals)
      where
        (addr, heap') = Heap.alloc (Fun compiledScomb'arity compiledScomb'code) heap

compileSc :: Scomb -> CompiledScomb
compileSc Scomb{..} = CompiledScomb
  { compiledScomb'name  = scomb'name
  , compiledScomb'arity = getArity env
  , compiledScomb'code  = code
  }
  where
    env   = initEnv scomb'args
    code  = compileR scomb'body env

compileR :: Expr -> Env -> Code
compileR expr env =
  compileC expr env <> Code.fromList [Slide (getArity env + 1), Unwind]

compileC :: Expr -> Env -> Code
compileC expr env = case expr of
  EVar v  -> Code.singleton $ case lookupEnv v env of
               Just n  -> Push n
               Nothing -> PushGlobal v
  ENum n  -> Code.singleton $ PushInt n
  EAp a b -> compileC b env <> compileC a (argOffset 1 env) <> Code.singleton Mkap


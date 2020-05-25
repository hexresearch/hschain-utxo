module Hschain.Utxo.Lang.Core.Compile.Prog(
    CoreProg
  , Scomb(..)
  , Expr(..)
  , CompiledScomb(..)
  , compile
  , compileSc
) where

import Data.Map.Strict (Map)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Core.Gmachine
import Hschain.Utxo.Lang.Core.Compile.Env

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..))
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Globals, Node(..))
import Hschain.Utxo.Lang.Core.Data.Utils

import qualified Data.List       as L
import qualified Data.Map.Strict as M

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
  | ELet [(Name, Expr)] Expr
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
  , gmachine'dump    = mempty
  , gmachine'stats   = Stat.empty
  }
  where
    (heap, globals) = buildInitHeap prog

buildInitHeap :: CoreProg -> (Heap, Globals)
buildInitHeap prog = (heap, Heap.initGlobals globalElems)
  where
    compiled = compiledPrimitives ++ fmap compileSc prog

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
  compileE expr env <> Code.fromList endInstrs
  where
    arity = getArity env

    endInstrs
      | arity == 0 = [Update 0, Unwind]
      | otherwise  = [Update arity, Pop arity, Unwind]

compileE :: Expr -> Env -> Code
compileE expr env = case expr of
  ENum n -> Code.singleton $ PushInt n
  ELet es e -> compileLet env es e
  EAp (EAp (EAp (EVar "if") a) b) c -> compileIf a b c
  EAp (EVar "negate") a             -> compileNegate a
  EAp (EAp (EVar op) a) b                  -> compileDiadic op a b
  _ -> defaultCase
  where
    compileDiadic op a b =
      case M.lookup op builtInDiadic of
        Just instr -> compileE b env <> compileE a (argOffset 1 env) <> Code.singleton instr
        Nothing    -> defaultCase

    compileIf a b c = compileE a env <> Code.singleton (Cond (compileE b env) (compileE c env))

    compileNegate a = compileE a env <> Code.singleton Neg

    defaultCase = compileC expr env <> Code.singleton Eval

compileC :: Expr -> Env -> Code
compileC expr env = case expr of
  EVar v  -> Code.singleton $ case lookupEnv v env of
               Just n  -> Push n
               Nothing -> PushGlobal v
  ENum n  -> Code.singleton $ PushInt n
  EAp a b -> compileC b env <> compileC a (argOffset 1 env) <> Code.singleton Mkap
  ELet es e -> compileLet env es e

compileLet :: Env -> [(Name, Expr)] -> Expr -> Code
compileLet env defs e =
  lets <> compileE e env' <> Code.singleton (Slide $ length defs)
  where
    lets = snd $ foldr (\(name, expr) (curEnv, code) -> (argOffset 1 curEnv, compileC expr curEnv <> code) ) (env, mempty) defs

    env' = compileArgs defs env

compileArgs :: [(Name, Expr)] -> Env -> Env
compileArgs defs env =
  L.foldl' (\e x -> uncurry insertEnv x e) env' $ zip (fmap fst defs) [n - 1, n - 2 .. 0]
  where
    n = length defs
    env' = argOffset n env

compiledPrimitives :: [CompiledScomb]
compiledPrimitives =
  [ op2 "+" Add
  , op2 "*" Mul
  , op2 "-" Sub
  , op2 "/" Div
  , op1 "negate" Neg
  , op2 "==" Eq
  , op2 "/=" Ne
  , op2 "<"  Lt
  , op2 "<=" Le
  , op2 ">"  Gt
  , op2 ">=" Ge
  , ifOp
  ]
  where
    op2 name op = CompiledScomb
      { compiledScomb'name  = name
      , compiledScomb'arity = 2
      , compiledScomb'code  = Code.fromList
          [Push 1, Eval, Push 1, Eval, op, Update 2, Pop 2, Unwind]
      }

    op1 name op = CompiledScomb
      { compiledScomb'name  = name
      , compiledScomb'arity = 1
      , compiledScomb'code  = Code.fromList
          [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]
      }

    ifOp = CompiledScomb
      { compiledScomb'name  = "if"
      , compiledScomb'arity = 3
      , compiledScomb'code  = Code.fromList
          [ Push 0, Eval
          , Cond (Code.singleton (Push 1)) (Code.singleton (Push 2))
          , Update 3, Pop 3, Unwind ]
      }

builtInDiadic :: Map Name Instr
builtInDiadic = M.fromList
  [ ("+", Add)
  , ("*", Mul)
  , ("-", Sub)
  , ("/", Div)
  , ("==", Eq)
  , ("/=", Ne)
  , ("<", Lt)
  , ("<=", Le)
  , (">", Gt)
  , (">=", Ge)
  ]





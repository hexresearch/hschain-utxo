module Hschain.Utxo.Lang.Core.Compile.Prog(
    compile
  , compileSc
) where

import Hschain.Utxo.Lang.Core.Gmachine
import Hschain.Utxo.Lang.Core.Compile.Env
import Hschain.Utxo.Lang.Core.Compile.Primitives

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..), CaseMap, GlobalName(..))
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Globals)
import Hschain.Utxo.Lang.Core.Data.Node
import Hschain.Utxo.Lang.Core.Data.Prim

import Hschain.Utxo.Lang.Core.Compile.Expr

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.IntMap     as IM

import qualified Hschain.Utxo.Lang.Core.Data.Code as Code
import qualified Hschain.Utxo.Lang.Core.Data.Heap as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Stat as Stat

-- | Compiles program to the G-machine for execution
compile :: CoreProg -> Gmachine
compile prog = Gmachine
  { gmachine'code    = Code.init
  , gmachine'stack   = mempty
  , gmachine'heap    = heap
  , gmachine'globals = globals
  , gmachine'dump    = mempty
  , gmachine'stats   = Stat.empty
  , gmachine'output  = mempty
  , gmachine'vstack  = mempty
  }
  where
    (heap, globals) = buildInitHeap prog

buildInitHeap :: CoreProg -> (Heap, Globals)
buildInitHeap prog = (heap, Heap.initGlobals globalElems)
  where
    compiled = fmap compileSc (primitives ++ prog)

    (heap, globalElems) = L.foldl' allocateSc (Heap.empty, []) compiled

    allocateSc (hp, globals) CompiledScomb{..} = (hp', (GlobalName compiledScomb'name, addr) : globals)
      where
        (addr, hp') = Heap.alloc (Fun compiledScomb'arity compiledScomb'code) hp

-- | Compile supercombinator.
compileSc :: Scomb -> CompiledScomb
compileSc Scomb{..} = CompiledScomb
  { compiledScomb'name  = scomb'name
  , compiledScomb'arity = getArity env
  , compiledScomb'code  = code
  }
  where
    env   = initEnv $ fmap typed'value scomb'args
    code  = compileR (typed'value scomb'body) env (getArity env)

compileR :: Expr -> Env -> Int -> Code
compileR expr env arity =
  case expr of
    ELet es e                         -> compileLetR env arity (fmap stripLetType es) e
    EAp (EAp (EAp (EVar "if") a) b) c -> compileIf a b c
    ECase e alts                      -> compileCaseR (typed'value e) alts
    _                                 -> defaultCase
  where
    endInstrs
      | arity == 0 = [Update 0, Unwind]
      | otherwise  = [Update arity, Pop arity, Unwind]

    defaultCase =
      case lastInstr of
        Just MkPrim -> updatePrim
        _           -> code <> Code.fromList endInstrs
      where
        code = compileE expr env
        (lastInstr, prefCode) = Code.splitLastInstr code
        updatePrim = prefCode <> Code.singleton (UpdatePrim arity) <> (Code.fromList $ tail endInstrs)

    compileIf a b c =
      compileB a env <> Code.singleton (Cond (compileR b env arity) (compileR c env arity))

    compileCaseR e alts = compileE e env <> Code.singleton (CaseJump $ compileAltsR arity env alts)

stripLetType :: (Typed Name, Expr) -> (Name, Expr)
stripLetType (n, e) = (typed'value n, e)

compileLetR :: Env -> Int -> [(Name, Expr)] -> Expr -> Code
compileLetR env arity defs e =
  lets <> compileR e env' (arity + length defs)
  where
    lets = snd $ foldr (\(_, expr) (curEnv, code) -> (argOffset 1 curEnv, compileC expr curEnv <> code) ) (env, mempty) defs
    env' = compileArgs defs env


compileE :: Expr -> Env -> Code
compileE expr env = case expr of
  EPrim n -> Code.singleton $ PushPrim n
  ELet es e -> compileLet env (fmap stripLetType es) e
  EAp (EAp (EAp (EVar "if") a) b) c -> compileIf a b c
  EAp (EAp (EVar op) a) b           -> compileDiadic op a b
  EAp (EVar op) a                   -> compileUnary op a
  ECase e alts -> compileCase env (typed'value e) alts
  EConstr tag arity -> Code.singleton $ PushGlobal $ ConstrName tag arity
  _ -> defaultCase
  where
    compileDiadic op a b =
      case M.lookup op builtInDiadic of
        Just instr -> compileDiadicInstrB env instr a b
                        <> Code.singleton MkPrim
        Nothing    -> defaultCase

    compileUnary op a =
      case M.lookup op builtInUnary of
        Just instr -> compileUnaryInstrB env instr a <> Code.singleton MkPrim
        Nothing    -> defaultCase

    compileIf a b c = compileB a env <> Code.singleton (Cond (compileE b env) (compileE c env))

    defaultCase = compileC expr env <> Code.singleton Eval

compileCase :: Env -> Expr -> [CaseAlt] -> Code
compileCase env e alts = compileE e env <> Code.singleton (CaseJump $ compileAlts env alts)

compileC :: Expr -> Env -> Code
compileC expr env = case expr of
  EVar v  -> Code.singleton $ case lookupEnv v env of
               Just n  -> Push n
               Nothing -> PushGlobal (GlobalName v)
  EPrim n  -> Code.singleton $ PushPrim n
  EAp a b -> compileC b env <> compileC a (argOffset 1 env) <> Code.singleton Mkap
  ELet es e -> compileLet env (fmap stripLetType es) e
  EConstr tag arity -> Code.singleton $ PushGlobal $ ConstrName tag arity
  ECase e alts -> compileCase env (typed'value e) alts
  -- TODO: we need to substitute it with special case
  -- see discussion at the book on impl at p. 136 section: 3.8.7

compileLet :: Env -> [(Name, Expr)] -> Expr -> Code
compileLet env defs e =
  lets <> compileE e env' <> Code.singleton (Slide $ length defs)
  where
    lets = snd $ foldr (\(_, expr) (curEnv, code) -> (argOffset 1 curEnv, compileC expr curEnv <> code) ) (env, mempty) defs

    env' = compileArgs defs env

compileArgs :: [(Name, Expr)] -> Env -> Env
compileArgs defs env =
  L.foldl' (\e x -> uncurry insertEnv x e) env' $ zip (fmap fst defs) [n - 1, n - 2 .. 0]
  where
    n = length defs
    env' = argOffset n env

compileAltsR :: Int -> Env -> [CaseAlt] -> CaseMap
compileAltsR arity = compileAltsBy compileR'
  where
    compileR' :: Int -> Expr -> Env -> Code
    compileR' offset expr env =
         Code.singleton (Split offset)
      <> compileR expr env (offset + arity)

compileAlts :: Env -> [CaseAlt] -> CaseMap
compileAlts = compileAltsBy compileE'
  where
    compileE' :: Int -> Expr -> Env -> Code
    compileE' offset expr env =
        Code.singleton (Split offset)
      <> compileE expr env
      <> Code.singleton (Slide offset)

compileAltsBy :: (Int -> Expr -> Env -> Code) -> Env -> [CaseAlt] -> CaseMap
compileAltsBy comp env alts =
  IM.fromList $ fmap (compileAlt comp env) alts

compileAlt :: (Int -> Expr -> Env -> Code) -> Env -> CaseAlt -> (Int, Code)
compileAlt comp env CaseAlt{..} = (caseAlt'tag, comp arity caseAlt'rhs env')
  where
    arity = length caseAlt'args

    env' = L.foldl' (\e (n, arg) -> insertEnv arg n e) (argOffset arity env) $ zip [0..] (fmap typed'value caseAlt'args)


compileB :: Expr -> Env -> Code
compileB expr env = case expr of
  EPrim n                           -> Code.singleton $ PushBasic n
  ELet es e                         -> compileLetB env (fmap stripLetType es) e
  EAp (EAp (EAp (EVar "if") a) b) c -> compileIf a b c
  EAp (EAp (EVar op) a) b           -> compileDiadic op a b
  EAp (EVar op) a                   -> compileUnary op a
  _                                 -> defaultCase
  where
    compileIf a b c =
      compileB a env <> Code.singleton (Cond (compileB b env) (compileB c env))

    compileDiadic op a b =
      case M.lookup op builtInDiadic of
        Just instr -> compileDiadicInstrB env instr a b
        Nothing    -> defaultCase

    compileUnary op a =
      case M.lookup op builtInUnary of
        Just instr -> compileUnaryInstrB env instr a
        Nothing    -> defaultCase

    defaultCase = compileE expr env <> Code.singleton Get

compileDiadicInstrB :: Env -> Instr -> Expr -> Expr -> Code
compileDiadicInstrB env instr a b =
  compileB b env <> compileB a env <> Code.singleton instr

compileUnaryInstrB :: Env -> Instr -> Expr -> Code
compileUnaryInstrB env instr a =
  compileB a env <> Code.singleton instr

compileLetB :: Env -> [(Name, Expr)] -> Expr -> Code
compileLetB env defs e =
  lets <> compileB e env' <> Code.singleton (Pop $ length defs)
  where
    lets = snd $ foldr (\(_, expr) (curEnv, code) -> (argOffset 1 curEnv, compileC expr curEnv <> code) ) (env, mempty) defs

    env' = compileArgs defs env



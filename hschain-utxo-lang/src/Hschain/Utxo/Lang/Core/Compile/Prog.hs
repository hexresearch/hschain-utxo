-- | Functions to compile core progrmamms to instructions of G-machine
module Hschain.Utxo.Lang.Core.Compile.Prog(
    compile
  , compileSc
  , coreProgTerminates
  , isSigmaScript
  , execScriptToSigma
) where

import Control.Applicative

import Data.Fix

import Hschain.Utxo.Lang.Core.Gmachine
import Hschain.Utxo.Lang.Core.Compile.Env
import Hschain.Utxo.Lang.Core.Compile.Primitives

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..), CaseMap, GlobalName(..))
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Globals)
import Hschain.Utxo.Lang.Core.Data.Node
import Hschain.Utxo.Lang.Core.Data.Prim

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.RecursionCheck
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (TxEnv)

import qualified Data.List       as L
import qualified Data.Map.Strict as M
import qualified Data.IntMap     as IM
import qualified Data.Vector     as V

import qualified Hschain.Utxo.Lang.Core.Data.Code as Code
import qualified Hschain.Utxo.Lang.Core.Data.Heap as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Output as Output
import qualified Hschain.Utxo.Lang.Core.Data.Stat as Stat
import qualified Hschain.Utxo.Lang.Error as E

-- | Executes script to sigma-expression.
--
-- Sigma script should contain main function that
-- returns sigma-expression. The script should be well-typed and
-- contain no recursion.
execScriptToSigma :: TxEnv -> CoreProg -> Either E.Error (Sigma PublicKey)
execScriptToSigma env prog = case isSigmaScript prog of
  Nothing  -> either (Left . E.ExecError . E.GmachineError) getSigmaOutput $ eval $ compile $ removeDeadCode $ addPrelude env prog
  Just err -> Left err
  where
    getSigmaOutput st = case Output.toList $ gmachine'output st of
      [PrimSigma sigma] -> Right sigma
      [PrimBool b]      -> Right $ Fix $ SigmaBool b
      _                 -> Left $ E.CoreScriptError E.ResultIsNotSigma

addPrelude :: TxEnv -> CoreProg -> CoreProg
addPrelude txEnv prog = preludeLib txEnv <> prog

-- | TODO: implement the function to remove unreachable code.
-- We start from main and then include only functions that are needed by finding free variables.
-- or maybe we can include it as a filter in prelude import.
removeDeadCode :: CoreProg -> CoreProg
removeDeadCode = id

-- | the program is sigma script if
--
-- * it terminates
-- * main function returns sigma-expression
isSigmaScript :: CoreProg -> Maybe E.Error
isSigmaScript prog =
      coreProgTerminates prog
  <|> mainIsSigma prog

-- | Check that program terminates.
--
-- It should
--
-- * be well typed
-- * has no recursion
coreProgTerminates :: CoreProg -> Maybe E.Error
coreProgTerminates prog =
      coreTypeError   (typeCheck preludeTypeContext prog)
  <|> recursiveScript (recursionCheck prog)
  where
    coreTypeError   = fmap (E.CoreScriptError . E.TypeCoreError)
    recursiveScript = E.wrapBoolError (E.CoreScriptError E.RecursiveScript )

mainIsSigma :: CoreProg -> Maybe E.Error
mainIsSigma (CoreProg prog) =
  case L.find (\sc -> scomb'name sc == "main") prog of
    Just mainComb -> resultIsNotSigma $ hasNoArgs mainComb && resultIsSigma mainComb
    Nothing       -> Just $ E.CoreScriptError E.NoMainFunction
  where
    resultIsNotSigma = E.wrapBoolError (E.CoreScriptError E.ResultIsNotSigma)
    hasNoArgs Scomb{..} = V.null scomb'args
    resultIsSigma Scomb{..} = sigmaT == typed'type scomb'body


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
buildInitHeap (CoreProg prog) = (heap, Heap.initGlobals globalElems)
  where
    compiled = fmap compileSc prog

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

compileR :: ExprCore -> Env -> Int -> Code
compileR expr env arity =
  case expr of
    ELet es e                         -> compileLetR env arity es e
    EIf a b c                         -> compileIf a b c
    ECase e alts                      -> compileCaseR e alts
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

compileLetR :: Env -> Int -> [(Name, ExprCore)] -> ExprCore -> Code
compileLetR env arity defs e =
  lets <> compileR e env' (arity + length defs)
  where
    lets = snd $ foldr (\(_, expr) (curEnv, code) -> (argOffset 1 curEnv, compileC expr curEnv <> code) ) (env, mempty) defs
    env' = compileArgs defs env

-- | Compile expression in strict context
compileE :: ExprCore -> Env -> Code
compileE expr env = case expr of
  EPrim n -> Code.singleton $ PushPrim n
  ELet es e -> compileLet env es e
  EIf a b c                         -> compileIf a b c
  EAp (EAp (EVar op) a) b           -> compileDiadic op a b
  EAp (EAp (EPolyVar op _) a) b     -> compileDiadic op a b
  EAp (EVar op) a                   -> compileUnary op a
  EAp (EPolyVar op _) a             -> compileUnary op a
  ECase e alts -> compileCase env e alts
  EConstr _ tag arity -> Code.singleton $ PushGlobal $ ConstrName tag arity
  --
  EVar{}     -> defaultCase
  EPolyVar{} -> defaultCase
  EAp{}      -> defaultCase
  EBottom    -> defaultCase
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

compileCase :: Env -> ExprCore -> [CaseAlt] -> Code
compileCase env e alts = compileE e env <> Code.singleton (CaseJump $ compileAlts env alts)

-- | Compile expression in lazy context
compileC :: ExprCore -> Env -> Code
compileC expr env = case expr of
  EVar v              -> fromVar v
  EPolyVar v _        -> fromVar v
  EPrim n             -> Code.singleton $ PushPrim n
  EAp a b             -> compileC b env <> compileC a (argOffset 1 env) <> Code.singleton Mkap
  ELet es e           -> compileLet env es e
  EConstr _ tag arity -> Code.singleton $ PushGlobal $ ConstrName tag arity
  ECase e alts        -> compileCase env e alts
  EIf a b c           -> compileIf a b c
  EBottom             -> Code.singleton Bottom
  -- TODO: we need to substitute it with special case
  -- see discussion at the book on impl at p. 136 section: 3.8.7
  where
    compileIf a b c = compileB a env <> Code.singleton (Cond (compileE b env) (compileE c env))

    fromVar v =
      Code.singleton $ case lookupEnv v env of
        Just n  -> Push n
        Nothing -> PushGlobal (GlobalName v)

compileLet :: Env -> [(Name, ExprCore)] -> ExprCore -> Code
compileLet env defs e =
  lets <> compileE e env' <> Code.singleton (Slide $ length defs)
  where
    lets = snd $ foldr (\(_, expr) (curEnv, code) -> (argOffset 1 curEnv, compileC expr curEnv <> code) ) (env, mempty) defs

    env' = compileArgs defs env

compileArgs :: [(Name, ExprCore)] -> Env -> Env
compileArgs defs env =
  L.foldl' (\e x -> uncurry insertEnv x e) env' $ zip (fmap fst defs) [n - 1, n - 2 .. 0]
  where
    n = length defs
    env' = argOffset n env

compileAltsR :: Int -> Env -> [CaseAlt] -> CaseMap
compileAltsR arity = compileAltsBy compileR'
  where
    compileR' :: Int -> ExprCore -> Env -> Code
    compileR' offset expr env =
         Code.singleton (Split offset)
      <> compileR expr env (offset + arity)

compileAlts :: Env -> [CaseAlt] -> CaseMap
compileAlts = compileAltsBy compileE'
  where
    compileE' :: Int -> ExprCore -> Env -> Code
    compileE' offset expr env =
        Code.singleton (Split offset)
      <> compileE expr env
      <> Code.singleton (Slide offset)

compileAltsBy :: (Int -> ExprCore -> Env -> Code) -> Env -> [CaseAlt] -> CaseMap
compileAltsBy comp env alts =
  IM.fromList $ fmap (compileAlt comp env) alts

compileAlt :: (Int -> ExprCore -> Env -> Code) -> Env -> CaseAlt -> (Int, Code)
compileAlt comp env CaseAlt{..} = (caseAlt'tag, comp arity caseAlt'rhs env')
  where
    arity = length caseAlt'args

    env' = L.foldl' (\e (n, arg) -> insertEnv arg n e) (argOffset arity env) $ zip [0..] (fmap typed'value caseAlt'args)


compileB :: ExprCore -> Env -> Code
compileB expr env = case expr of
  EPrim n                           -> Code.singleton $ PushBasic n
  ELet es e                         -> compileLetB env es e
  EIf a b c                         -> compileIf a b c
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

compileDiadicInstrB :: Env -> Instr -> ExprCore -> ExprCore -> Code
compileDiadicInstrB env instr a b =
  compileB b env <> compileB a env <> Code.singleton instr

compileUnaryInstrB :: Env -> Instr -> ExprCore -> Code
compileUnaryInstrB env instr a =
  compileB a env <> Code.singleton instr

compileLetB :: Env -> [(Name, ExprCore)] -> ExprCore -> Code
compileLetB env defs e =
  lets <> compileB e env' <> Code.singleton (Pop $ length defs)
  where
    lets = snd $ foldr (\(_, expr) (curEnv, code) -> (argOffset 1 curEnv, compileC expr curEnv <> code) ) (env, mempty) defs

    env' = compileArgs defs env


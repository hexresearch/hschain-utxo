-- | Evaluation of G-machine code
module Hschain.Utxo.Lang.Core.Gmachine.Eval(
  eval
) where

import Data.Fix

import Hschain.Utxo.Lang.Sigma (SigmaF(..))
import Hschain.Utxo.Lang.Core.Gmachine.Monad

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..), CaseMap, GlobalName(..))
import Hschain.Utxo.Lang.Core.Data.Node
import Hschain.Utxo.Lang.Core.Data.Prim

import Hschain.Utxo.Lang.Core.Gmachine.Eval.Dump
import Hschain.Utxo.Lang.Core.Gmachine.Eval.Heap
import Hschain.Utxo.Lang.Core.Gmachine.Eval.Prim
import Hschain.Utxo.Lang.Core.Gmachine.Eval.Stack
import Hschain.Utxo.Lang.Core.Gmachine.Eval.Vstack

import qualified Data.Sequence as Seq

import qualified Hschain.Utxo.Lang.Core.Data.Code   as Code
import qualified Hschain.Utxo.Lang.Core.Data.Heap   as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Output as Output
import qualified Hschain.Utxo.Lang.Core.Data.Stack  as Stack
import qualified Hschain.Utxo.Lang.Core.Data.Stat   as Stat


-- | Evaluates code for Gmachine and returns the final state
-- and possible errors. If there are no errors then code was successfully executed.
eval :: Gmachine -> Either Error Gmachine
eval = runExec $ loop >> modifyOutput Output.simplifySigmas
  where
    loop = fix $ \rec -> do
      mCode <- getNextInstr
      case mCode of
        Just code -> do
          updateStats
          dispatch code
          rec
        Nothing   -> stop

    stop = return ()

-- | React on single instruction
dispatch :: Instr -> Exec ()
dispatch = \case
  PushGlobal n -> pushGlobal n
  PushPrim n   -> pushPrim n
  PushBasic n  -> pushBasic n
  Push n       -> push n
  Mkap         -> mkap
  Unwind       -> unwind
  Update n     -> update n
  Pop n        -> pop n
  Slide n      -> slide n
  Alloc n      -> allocEmptyNodes n
  Eval         -> evalExpr
  -- numeric
  Add          -> binNumOp (+)
  Sub          -> binNumOp (\a b -> a - b)
  Mul          -> binNumOp (*)
  Div          -> binNumOp div
  Neg          -> negOp
  -- compare
  Eq           -> compareOp (==)
  Ne           -> compareOp (/=)
  Lt           -> compareOp (<)
  Le           -> compareOp (<=)
  Gt           -> compareOp (>)
  Ge           -> compareOp (>=)
  -- booleans (encoded with integers)
  And          -> binBoolOp (&&)
  Or           -> binBoolOp (||)
  Xor          -> binBoolOp  (/=)
  Not          -> notOp
  -- text
  TextAppend   -> binTextOp mappend
  TextLength   -> textLength
  HashBlake    -> hashBlake
  HashSha      -> hashSha
  ShowInt      -> showInt
  ShowBool     -> showBool
  -- bytes
  BytesAppend  -> binBytesOp mappend
  ToBytes tag  -> serialiseToBytes tag
  FromBytes tag -> deserialiseFromBytes tag
  Sha256       -> hashSha
  -- sigma expressions
  SigAnd       -> binSigmaOp (\a b -> Fix $ SigmaAnd [a, b])
  SigOr        -> binSigmaOp (\a b -> Fix $ SigmaOr  [a, b])
  SigPk        -> pkOp
  SigBool      -> boolToSigmaOp
  -- conditionals
  Cond c1 c2   -> cond c1 c2
  Pack m n     -> pack m n
  CaseJump as  -> caseJump as
  Split n      -> split n
  Print        -> printExpr
  MkPrim       -> mkPrim
  Get          -> getExpr
  UpdatePrim p -> updatePrim p
  Bottom       -> throwError BottomTerm

pushGlobal :: GlobalName -> Exec ()
pushGlobal = \case
  GlobalName n   -> pushGlobalName n
  ConstrName m n -> pushConstrName m n

pushGlobalName :: Name -> Exec ()
pushGlobalName name = do
  addr <- fromError (NotFound name) $ fmap (Heap.lookupGlobalScomb (GlobalName name)) getGlobals
  putAddr addr

pushConstrName :: Int -> Int -> Exec ()
pushConstrName tag arity = do
  mAddr <- fmap (Heap.lookupGlobalScomb (ConstrName tag arity)) getGlobals
  case mAddr of
    Just addr -> putAddr addr
    Nothing   -> putAddr =<< initGlobalConstrName tag arity

initGlobalConstrName :: Int -> Int -> Exec Addr
initGlobalConstrName tag arity = do
  addr <- alloc (Fun arity (Code.fromList [Pack tag arity, Update 0, Unwind]))
  modifyGlobals $ Heap.insertGlobalScomb (ConstrName tag arity) addr
  return addr

pushPrim :: Prim -> Exec ()
pushPrim prim = do
  mAddr <- fmap (Heap.lookupGlobalConst prim) getGlobals
  putAddr =<< maybe onMissing pure mAddr
  where
    onMissing = do
      addr <- alloc (NodePrim prim)
      modifyGlobals $ Heap.insertGlobalConst prim addr
      return addr

mkap :: Exec ()
mkap = do
  a1  <- popAddr
  a2  <- popAddr
  res <- alloc (Ap a1 a2)
  putAddr res

unwind :: Exec ()
unwind = do
  addr <- peekAddr
  node <- lookupHeap addr
  newState addr node
  where
    newState addr = \case
      NodePrim _ -> onPrim addr
      Ap a1 _   -> onAp a1
      Fun size code -> onFun size code
      NodeInd indAddr -> onInd indAddr
      NodeConstr _ _ -> onConstr addr

    onPrim   = returnVal
    onConstr = returnVal

    onAp addr = do
      putAddr addr
      modifyCode (Code.singleton Unwind <>)

    onFun size code = do
      len <- getStackSize
      if (len > size)
        then do
          heap  <- getHeap
          stack <- getStack
          maybe stackIsEmpty putStack $ Stack.rearrange size heap stack
          modifyCode (code <>)
        else do
          lastVal <- peekBottomAddr
          loadDump
          putAddr lastVal

    returnVal topAddr = do
      loadDump
      putAddr topAddr

    -- | Substitute top of the stack with indirection address
    onInd a = do
      _ <- popAddr
      putAddr a
      modifyCode (Code.singleton Unwind <> )

update :: Int -> Exec ()
update n = do
  topAddr <- popAddr
  nAddr <- lookupAddr n
  modifyHeap $ Heap.insertNode nAddr (NodeInd topAddr)

allocEmptyNodes :: Int -> Exec ()
allocEmptyNodes n = do
  as <- mapM (const $ alloc emptyNode) $ Seq.fromList [1 .. n]
  modifyStack $ Stack.appendSeq as
  where
    emptyNode = NodeInd (-1)

evalExpr :: Exec ()
evalExpr = do
  topAddr <- popAddr
  saveDump
  putCode  $ Code.singleton Unwind
  putStack $ Stack.singleton topAddr

------------------------------------------------------

cond :: Code -> Code -> Exec ()
cond c1 c2 = do
  predicate <- popBool
  let code = if predicate then c1 else c2
  modifyCode (code <> )

-------------------------------------------------------------
-- execution of case-expression and custom data types

pack :: Int -> Int -> Exec ()
pack tagId arity = do
  args <- popAddrList arity
  topAddr <- alloc $ NodeConstr tagId args
  putAddr topAddr

caseJump :: CaseMap -> Exec ()
caseJump caseMap = do
  node <- lookupHeap =<< peekAddr
  case node of
    NodeConstr tagId _ -> do
      code <- maybe missingCase pure $ Code.getCaseCode tagId caseMap
      modifyCode (code <> )
    _ -> badType

split :: Int -> Exec ()
split arity = do
  node <- lookupHeap =<< popAddr
  case node of
    NodeConstr _ args -> modifyStack (Stack.appendSeq $ Seq.take arity args)
    _ -> badType


printExpr :: Exec ()
printExpr = do
  node <- lookupHeap =<< popAddr
  case node of
    NodePrim p        -> modifyOutput (Output.put p)
    NodeConstr _ args -> do
      modifyStack (Stack.appendSeq args)
      modifyCode  (Code.appendSeq $ printN args)
    _ -> error (show node)
  where
    printN args = foldMap (const cmds) args
      where
        cmds = Seq.fromList [Eval, Print]

-------------------------------------------------------------
-- state update proxies for G-machine units

-- code

-- | read next code instruction to execute
getNextInstr :: Exec (Maybe Instr)
getNextInstr = do
  (mInstr, code') <- fmap Code.next getCode
  putCode code'
  return mInstr

-- stats

-- | Updates program execution statistics.
-- Call it on every command execution.
updateStats :: Exec ()
updateStats = modifyStats Stat.update



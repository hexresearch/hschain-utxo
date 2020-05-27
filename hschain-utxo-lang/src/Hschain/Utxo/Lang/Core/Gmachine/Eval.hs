-- | Evaluation of G-machine code
module Hschain.Utxo.Lang.Core.Gmachine.Eval(
  eval
) where

import Data.Text (Text)

import Debug.Trace

import Hschain.Utxo.Lang.Core.Gmachine.Monad

import Hschain.Utxo.Lang.Core.Data.Code (Code, Instr(..), CaseMap, GlobalName(..))
import Hschain.Utxo.Lang.Core.Data.Heap (Heap, Node(..), Globals)
import Hschain.Utxo.Lang.Core.Data.Stack (Stack)
import Hschain.Utxo.Lang.Core.Data.Stat (Stat)
import Hschain.Utxo.Lang.Core.Data.Utils

import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import qualified Data.Text as T

import qualified Hschain.Utxo.Lang.Core.Data.Code   as Code
import qualified Hschain.Utxo.Lang.Core.Data.Dump   as Dump
import qualified Hschain.Utxo.Lang.Core.Data.Heap   as Heap
import qualified Hschain.Utxo.Lang.Core.Data.Output as Output
import qualified Hschain.Utxo.Lang.Core.Data.Stack  as Stack
import qualified Hschain.Utxo.Lang.Core.Data.Stat   as Stat
import qualified Hschain.Utxo.Lang.Core.Data.Vstack as Vstack


-- | Evaluates code for Gmachine and returns the final state
-- and possible errors. If there are no errors then code was successfully executed.
eval :: Gmachine -> Either Error Gmachine
eval = runExec loop
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
  PushInt n    -> pushInt n
  PushText t   -> pushText t
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
  Eq           -> condOp (==)
  Ne           -> condOp (/=)
  Lt           -> condOp (<)
  Le           -> condOp (<=)
  Gt           -> condOp (>)
  Ge           -> condOp (>=)
  -- booleans (encoded with integers)
  And          -> binNumOp min
  Or           -> binNumOp max
  Xor          -> binNumOp (\a b -> boolToInt (a /= b))
  Not          -> notOp
  -- text
  TextAppend   -> binTextOp mappend
  TextLength   -> textLength
  HashBlake    -> hashBlake
  HashSha      -> hashSha
  -- conditionals
  Cond c1 c2   -> cond c1 c2
  Pack m n     -> pack m n
  CaseJump as  -> caseJump as
  Split n      -> split n
  Print        -> printExpr
  MkInt        -> mkInt
  MkBool       -> mkBool
  MkText       -> mkText
  Get          -> getExpr
  UpdateInt n  -> updateInt n
  UpdateBool n -> updateBool n
  UpdateText n -> updateText n

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

pushInt :: Int -> Exec ()
pushInt num = do
  mAddr <- fmap (Heap.lookupGlobalConst num) getGlobals
  putAddr =<< maybe onMissing pure mAddr
  where
    onMissing = do
      addr <- alloc (NodeInt num)
      modifyGlobals $ Heap.insertGlobalConst num addr
      return addr

pushText :: Text -> Exec ()
pushText = undefined

pushBasic :: Int -> Exec ()
pushBasic n = putVstack n

mkap :: Exec ()
mkap = do
  a1  <- popAddr
  a2  <- popAddr
  res <- alloc (Ap a1 a2)
  putAddr res

push :: Int -> Exec ()
push n =
  putAddr =<< lookupAddr n

slide :: Int -> Exec ()
slide n = modifyStack (Stack.slide n)

unwind :: Exec ()
unwind = do
  addr <- peekAddr
  node <- lookupHeap addr
  newState addr node
  where
    newState addr = \case
      NodeInt n -> onNum addr
      Ap a1 _   -> onAp a1
      Fun size code -> onFun size code
      NodeInd addr -> onInd addr
      NodeConstr _ _ -> onConstr addr

    onNum    = returnVal
    onConstr = returnVal

    readNodeArg addr = do
      node <- lookupHeap addr
      case node of
        Ap _ argAddr -> return argAddr
        _            -> badType

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

pop :: Int -> Exec ()
pop n = modifyStack $ Stack.drop n

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
-- shortcuts for sequence [MkInt, Update n] and [MkBool, Update n]

updatePrimBy :: Exec a -> (a -> Node) -> Int -> Exec ()
updatePrimBy getter toNode n = do
  topAddr <- alloc . toNode =<< getter
  nAddr <- lookupAddr n
  modifyHeap $ Heap.insertNode nAddr (NodeInd topAddr)

updateInt :: Int -> Exec ()
updateInt = updatePrimBy popVstack NodeInt

updateBool :: Int -> Exec ()
updateBool = updatePrimBy popVstack NodeInt

updateText :: Int -> Exec ()
updateText = updatePrimBy popText NodeText

------------------------------------------------------
-- V-stack operations

mkBy :: Exec a -> (a -> Node) -> Exec ()
mkBy getter toNode = do
  addr <- alloc . toNode =<< getter
  putAddr addr

mkInt :: Exec ()
mkInt = mkBy popVstack NodeInt

-- | We represent booleans with integers so it's the same as mkInt
mkBool :: Exec ()
mkBool = mkBy popVstack NodeInt

mkText :: Exec ()
mkText = mkBy popText NodeText

getExpr :: Exec ()
getExpr = do
  node <- lookupHeap =<< popAddr
  case node of
    NodeInt n  -> putVstack n
    NodeText t -> putText t
    _          -> badType

------------------------------------------------------
-- primitive operators

-- numbers

binNumOp :: (Int -> Int -> Int) -> Exec ()
binNumOp = primOp2 popVstack popVstack putVstack

negOp :: Exec ()
negOp = primOp1 popVstack putVstack negate

condOp :: (Int -> Int -> Bool) -> Exec ()
condOp op = binNumOp (\a b -> boolToInt $ op a b)

boolToInt :: Bool -> Int
boolToInt = \case
      True  -> 1
      False -> 0

intToBool :: Int -> Bool
intToBool n = n /= 0

notOp :: Exec ()
notOp = primOp1 popVstack putVstack (\x -> 1 - x)

popVstack :: Exec Int
popVstack = fromError VstackIsEmpty $ stateVstack Vstack.pop

putVstack :: Int -> Exec ()
putVstack n = modifyVstack (Vstack.put n)


primOp1 :: (Exec a) -> (b -> Exec ()) -> (a -> b) -> Exec ()
primOp1 unbox box f =
  box =<< fmap f unbox

primOp2 :: (Exec a) -> (Exec b) -> (c -> Exec ()) -> (a -> b -> c) -> Exec ()
primOp2 unboxA unboxB box f = do
  a <- unboxA
  b <- unboxB
  box $ f a b

cond :: Code -> Code -> Exec ()
cond c1 c2 = do
  n <- popVstack
  let code = if (n == 1) then c1 else c2
  modifyCode (code <> )

-- text

binTextOp :: (Text -> Text -> Text) -> Exec ()
binTextOp = primOp2 popText popText putText

popText :: Exec Text
popText = undefined

putText :: Text -> Exec ()
putText = undefined

textLength :: Exec ()
textLength = primOp1 popText putVstack T.length

hashBlake :: Exec ()
hashBlake = primOp1 popText putText getBlakeHash
  where
    getBlakeHash = undefined

hashSha :: Exec ()
hashSha = primOp1 popText putText getSha256
  where
    getSha256 = undefined

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
    NodeConstr _ args -> modifyStack (Stack.appendSeq args)
    _ -> badType


printExpr :: Exec ()
printExpr = do
  node <- lookupHeap =<< popAddr
  case node of
    NodeInt n         -> modifyOutput (Output.put n)
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

-- stack

-- | Puts address on top of the stack
putAddr :: Addr -> Exec ()
putAddr addr = modifyStack (Stack.put addr)

-- | Read top of the stack and remove that element from the stack
popAddr :: Exec Addr
popAddr = fromError StackIsEmpty $
  stateStack Stack.pop

-- | Pops specified number of arguments
--
-- TODO: re-implement with stack operation directly (for efficient execution)
popAddrList :: Int -> Exec (Seq Addr)
popAddrList size = stateStack $ Stack.popN size

-- | Read top of the stack without modifying it.
peekAddr :: Exec Addr
peekAddr = fromError StackIsEmpty $
  fmap Stack.peek getStack

peekBottomAddr :: Exec Addr
peekBottomAddr = fromError StackIsEmpty $
  fmap Stack.peekBottom getStack

-- | Read stack by index.
lookupAddr :: Int -> Exec Addr
lookupAddr n = fromError StackIsEmpty $
  fmap (Stack.lookup n) getStack

-- | Read stack size
getStackSize :: Exec Int
getStackSize = fmap Stack.length getStack

-- code

-- | Is final state of G-machine reached (no code left to execute)
isFinal :: Exec Bool
isFinal = fmap Code.null getCode

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

-- heap

-- | allocate new cell for the node on the heap,
alloc :: Node -> Exec Addr
alloc node = stateHeap (Heap.alloc node)

-- | Get the node stored in the heap by its address.
lookupHeap :: Addr -> Exec Node
lookupHeap addr = fromError (BadAddr addr) $
  fmap (Heap.lookup addr) getHeap

-- dump

loadDump :: Exec ()
loadDump = do
  (code, stack) <- popDump
  putCode code
  putStack stack

saveDump :: Exec ()
saveDump = do
  code <- getCode
  stack <- getStack
  insertDump code stack

popDump :: Exec (Code, Stack)
popDump = do
  dump <- getDump
  let (mRes, dump') = Dump.pop dump
  putDump dump'
  maybe dumpIsEmpty pure mRes

insertDump :: Code -> Stack -> Exec ()
insertDump code stack = modifyDump $ Dump.put code stack


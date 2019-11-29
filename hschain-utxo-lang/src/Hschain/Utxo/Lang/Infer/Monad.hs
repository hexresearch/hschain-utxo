module Hschain.Utxo.Lang.Infer.Monad (
    Infer
  , runInfer
  , fresh
  , infer
  , closeOver
) where

import Control.Applicative ((<|>))
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict
import Control.Monad.Writer

import Data.Fix hiding ((~>))
import Data.Text (Text)

import Safe

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Infer.Constraint
import Hschain.Utxo.Lang.Infer.Subst
import Hschain.Utxo.Lang.Infer.TypeEnv

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Vector as V


infixr 6 ~>

-- | Inference state
data InferState = InferState { count :: Int }

-- | Inference monad
type Infer a = (RWST
                  TypeEnv         -- Typing environment
                  [Constraint]
                  InferState
                  (Except         -- Inference errors
                    TypeError)
                  a)              -- Result

-- | Initial inference state
initInfer :: InferState
initInfer = InferState { count = 0 }


-- | Run the inference monad
runInfer :: TypeEnv -> Infer Type -> Either TypeError (Type, [Constraint])
runInfer env m = runExcept $ evalRWST m env initInfer

letters :: [Text]
letters = fmap T.pack $ [1..] >>= flip replicateM ['a'..'z']

fresh :: Infer Type
fresh = do
  s <- get
  put s{count = count s + 1}
  return $ Fix $ VarType $ TypeVar (letters !! count s)

-- | Unify two types
uniWith :: Lang -> Type -> Type -> Infer ()
uniWith expr t1 t2 = tell [(t1, t2, expr)]

-- | Extend type environment
inEnv :: (VarName, Scheme) -> Infer a -> Infer a
inEnv (x, sc) m = do
  let scope e = (remove e x) `extend` (x, sc)
  local scope m

instantiate ::  Scheme -> Infer Type
instantiate (Forall as t) = do
    as' <- mapM (const fresh) as
    let s = Subst $ M.fromList $ zip as as'
    return $ apply s t

generalize :: TypeEnv -> Type -> Scheme
generalize env t  = Forall as t
    where as = S.toList $ ftv t `S.difference` ftv env

-- | Canonicalize and return the polymorphic toplevel type.
closeOver :: Type -> Scheme
closeOver = normalize . generalize empty

infer :: Lang -> Infer Type
infer mainExpr@(Fix ex) = case ex of
  PrimE p           -> fromPrim p
  Var varName       -> lookupEnv varName
  Lam varName _ e   -> fromLam varName e
  LamList ts e      -> fromLamList ts e
  Apply e1 e2       -> fromApply e1 e2
  Let x e1 e2       -> fromLet x e1 e2
  LetArg v args a b -> fromLetArg v args a b
  If c t e          -> fromIf c t e
  UnOpE op e        -> fromUnOp op e
  BinOpE op e1 e2   -> fromBinOp op e1 e2
  LetRec v _ a e    -> error "LetRec undefined type-inference"
  Pk a              -> fromPk a
  Undef             -> fresh
  Trace str a       -> fromTrace str a
  Ascr a ty         -> fromAscr a ty
  TextE expr        -> fromText expr
  VecE vec          -> fromVec vec
  BoxE box          -> fromBox box
  GetEnv a          -> fromGetEnv a
  Tuple as          -> fromTuple as
  where
    uni = uniWith mainExpr

    fromPrim x = return $ Fix $ case x of
      PrimInt _    -> IntType
      PrimMoney _  -> MoneyType
      PrimDouble _ -> DoubleType
      PrimString _ -> StringType
      PrimBool _   -> BoolType

    fromLam x e = do
      tv <- fresh
      t <- inEnv (x, Forall [] tv) (infer e)
      return $ tv ~> t

    fromLamList vars e = infer $ unfoldLamList vars e

    unfoldLamList vars a = L.foldl' (\z a -> z . Fix . uncurry Lam a) id vars a

    fromApply e1 e2 = do
      t1 <- infer e1
      t2 <- infer e2
      tv <- fresh
      uni t1 (t2 ~> tv)
      return tv

    fromLet x e1 e2 = do
      env <- ask
      t1 <- infer e1
      let sc = generalize env t1
      t2 <- inEnv (x, sc) (infer e2)
      return t2

    fromLetArg v args a b = infer $ unfoldLetArg v args a b

    unfoldLetArg v args a b = Fix $ Let v (Fix $ LamList (fmap (, Fix UknownType) args) a) b

    fromIf cond tr fl = do
      t1 <- infer cond
      t2 <- infer tr
      t3 <- infer fl
      uni t1 typeBool
      uni t2 t3
      return t2

    fromUnOp op e = case op of
      Not -> inferOp1 (boolT ~> boolT) e
      Neg -> inferOp1 (intT ~> intT) e
      TupleAt n -> fromTupleAt n e

    fromTupleAt n e = do
      eT <- infer e
      tv <- fresh
      let err = throwError $ UnificationFail eT (Fix $ TupleType mempty) mainExpr -- todo better error message
      case eT of
        Fix (TupleType vs) -> case vs `atMay` n of
          Just elemT -> uni tv elemT
          Nothing    -> err
        _ -> err
      return tv

    inferOp1 resT arg = do
      argT <- infer arg
      tv <- fresh
      uni (argT ~> tv) resT
      return tv

    fromBinOp op e1 e2 = do
      t1 <- infer e1
      t2 <- infer e2
      tv <- fresh
      let u1 = t1 ~> (t2 ~> tv)
      u2 <- binOps op
      uni u1 u2
      return tv

    binOps = \case
      And                -> pure boolBinOp
      Or                 -> pure boolBinOp
      Plus               -> pure numBinOp <|> pure doubleOp
      Minus              -> pure numBinOp
      Times              -> pure numBinOp
      Div                -> pure numBinOp
      Equals             -> pure ordBinOp
      NotEquals          -> pure ordBinOp
      LessThan           -> pure ordBinOp
      GreaterThan        -> pure ordBinOp
      LessThanEquals     -> pure ordBinOp
      GreaterThanEquals  -> pure ordBinOp
      ComposeFun         -> do
        a <- fresh
        b <- fresh
        c <- fresh
        return ((a ~> b) ~> (b ~> c))
      where
        boolBinOp = boolT ~> (boolT ~> boolT)
        numBinOp  = intT  ~> (intT  ~> intT)
        ordBinOp  = intT  ~> (intT  ~> boolT)

        doubleOp = doubleT ~> doubleT ~> doubleT

    fromPk = inferOp1 (stringT ~> boolT)

    fromTrace str a = do
      strT <- infer str
      aT   <- infer a
      tv   <- fresh
      uni (strT ~> aT ~> tv) (stringT ~> aT ~> aT)
      return tv

    fromAscr a ty = do
      aT <- infer a
      uni aT ty
      return ty

    fromText = \case
      TextAppend a b -> fromTextAppend a b
      ConvertToText  -> fromConvertToText
      TextLength     -> return (stringT ~> intT)
      TextHash _     -> return (stringT ~> stringT)

    fromTextAppend a b = do
      aT <- infer a
      bT <- infer b
      tv <- fresh
      uni (aT ~> bT ~> tv) (stringT ~> stringT ~> stringT)
      return tv

    fromConvertToText = do
      t1 <- fresh
      t2 <- fresh
      uni (t1 ~> t2) (t1 ~> stringT)
      return (t1 ~> t2)

    fromVec = \case
      NewVec vs     -> fromNewVec vs
      VecAppend a b -> fromVecAppend a b
      VecAt a b     -> fromVecAt a b
      VecLength     -> fromVecLength
      VecMap        -> fromVecMap
      VecFold       -> fromVecFold
      where
        fromNewVec vs = do
          tv <- fresh
          vTs <- mapM infer vs
          mapM_ (uni tv) vTs
          return $ vecT tv

        fromVecAppend a b = do
          aT <- infer a
          bT <- infer b
          tv <- fresh
          uni (aT ~> bT ~> vecT tv) (vecT tv ~> vecT tv ~> vecT tv)
          return $ vecT tv

        fromVecAt a n = do
          aT <- infer a
          nT <- infer n
          tv <- fresh
          uni (aT ~> nT ~> tv) (vecT tv ~> intT ~> tv)
          return tv

        fromVecLength = do
          tv <- fresh
          return (vecT tv ~> intT)

        fromVecMap = do
          a <- fresh
          b <- fresh
          return ((a ~> b) ~> vecT a ~> vecT b)

        fromVecFold = do
          a <- fresh
          b <- fresh
          return ((a ~> b ~> a) ~> a ~> vecT b ~> a)

    fromBox = \case
      PrimBox _     -> return boxT
      BoxAt a field -> fromBoxField a field

    fromBoxField a field = do
      aT <- infer a
      uni aT boxT
      case field of
        BoxFieldId      -> pure stringT
        BoxFieldValue   -> pure moneyT
        BoxFieldScript  -> pure scriptT
        BoxFieldArg arg -> do
          argT <- infer arg
          uni argT stringT
          tv <- fresh
          uni (aT ~> argT ~> tv) (boxT ~> stringT ~> tv)
          return tv


    fromGetEnv = \case
      Height    -> return intT
      Input  a  -> fromBoxByIndex a
      Output a  -> fromBoxByIndex a
      Self      -> return boxT
      Inputs    -> return $ vecT boxT
      Outputs   -> return $ vecT boxT
      GetVar a  -> fromGetVar a
      where
        fromBoxByIndex a = do
          aT <- infer a
          tv <- fresh
          uni (aT ~> tv) (intT ~> boxT)
          return tv

        fromGetVar a = do
          aT <- infer a
          tv <- fresh
          uni (aT ~> tv) (stringT ~> tv)
          return tv

    fromTuple as = fmap tupleT $ mapM infer $ V.toList as

-- | Lookup type in the environment
lookupEnv :: VarName -> Infer Type
lookupEnv x = do
  (TypeEnv env) <- ask
  case M.lookup x env of
      Nothing   -> throwError $ UnboundVariable x
      Just s    -> do
          t <- instantiate s
          return t

normalize :: Scheme -> Scheme
normalize (Forall _ body) = Forall (map snd ord) (normtype body)
  where
    ord = zip (S.toList $ ftv body) (map TypeVar letters)

    normtype = cata $ \case
      VarType v -> case Prelude.lookup v ord of
        Just x -> Fix $ VarType x
        Nothing -> error "type variable not in signature"
      x -> Fix x

------------------------------------------
-- constants

typeBool :: Type
typeBool = Fix BoolType

boolT :: Type
boolT = Fix BoolType

intT :: Type
intT = Fix IntType

doubleT :: Type
doubleT = Fix DoubleType

stringT :: Type
stringT = Fix StringType

moneyT :: Type
moneyT = Fix MoneyType

scriptT :: Type
scriptT = Fix ScriptType

vecT :: Type -> Type
vecT = Fix . VectorType

tupleT :: [Type] -> Type
tupleT as = Fix $ TupleType as

boxT :: Type
boxT = Fix BoxType

(~>) :: Type -> Type -> Type
(~>) a b = Fix $ FunctionType a b


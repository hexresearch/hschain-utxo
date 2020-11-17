{-# LANGUAGE RankNTypes          #-}
-- | Evaluator for Core
module Hschain.Utxo.Lang.Core.RefEval
  ( Val(..)
  , EvalErr(..)
  , EvalResult(..)
  , evalProg
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Int
import Data.Bits       (xor)
import Data.ByteString (ByteString)
import Data.Bool
import Data.String
import Data.Text       (Text)
import Data.Typeable
import Data.Fix
import Data.Foldable (foldrM)
import qualified Data.Vector     as V
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy   as MapL
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LB

import HSChain.Crypto     (Hash(..),hashBlob,ByteRepr(..))
import HSChain.Crypto.SHA (SHA256)
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (Box(..),PostBox(..),BoxOutput(..),BoxInput(..),Args(..),ArgType(..),Script(..),BoxId(..),InputEnv(..))

import qualified Hschain.Utxo.Lang.Const as Const
import qualified Hschain.Utxo.Lang.Crypto.Signature as Crypto

-- | Value hanled by evaluator
data Val
  = ValP !Prim                            -- ^ Primitive value
  | ValF  (Val -> Eval Val)               -- ^ Unary function
  | Val2F (Val -> Val -> Eval Val)        -- ^ Binary function. Added in order to make defining primops easier
  | Val3F (Val -> Val -> Val -> Eval Val) -- ^ Ternary function. Added in order to make defining primops easier
  | ValCon Int [Val]                      -- ^ Constructor cell

instance Show Val where
  showsPrec n v
    = showParen (n >= 11)
    $ case v of
        ValP      p -> showsPrec 11 p
        ValF  _     -> showString "Function"
        Val2F _     -> showString "Function"
        Val3F _     -> showString "Function"
        ValCon i xs -> showString "CON "
                     . showsPrec 11 i
                     . showChar ' '
                     . showsPrec 11 xs

-- | Result of evaluation. It exisit in current form in order to be
--   able to test list based function.
data EvalResult
  = EvalPrim !Prim
  | EvalList [Prim]
  | EvalFail EvalErr
  deriving (Show, Eq)

-- | Evaluation error
data EvalErr
  = TypeMismatch    -- ^ Type error. Should never happen when evaluating well-typed program.
  | EvalErr String  -- ^ Some other error
  deriving (Show, Eq)

instance IsString EvalErr where
  fromString = EvalErr

-- | Local evaluation environment. Map from variable bindings to values
type LEnv = Map.Map Name Val

-- | Evaluate program
evalProg :: InputEnv -> ExprCore -> EvalResult
evalProg env prog =
  case runEval (evalExpr env mempty prog) of
    Right val -> case val of
                    ValP p      -> EvalPrim p
                    ValF{}      -> EvalFail $ EvalErr "Returning function"
                    Val2F{}     -> EvalFail $ EvalErr "Returning function"
                    Val3F{}     -> EvalFail $ EvalErr "Returning function"
                    ValCon i xs -> maybe (EvalFail $ EvalErr "Not a list") EvalList
                                $ con2list i xs
    Left err -> EvalFail err
  where
    con2list 0 []                   = Just []
    con2list 1 [ValP p,ValCon i xs] = (p :) <$> con2list i xs
    con2list _ _                    = Nothing

-- | Monad for evaluation of core expressions
newtype Eval a = Eval (StateT EvalEnv (Either EvalErr) a)
  deriving newtype (Functor, Applicative, Monad, MonadState EvalEnv, MonadError EvalErr)

-- | Evaluator internal state
data EvalEnv = EvalEnv
  { evalEnv'reductions :: !Int  -- ^ counter for amount of evaluated reductions so far
  }
  deriving (Show, Eq)

runEval :: Eval a -> Either EvalErr a
runEval (Eval st) = evalStateT st EvalEnv{ evalEnv'reductions = 0 }

getReductionCount :: Eval Int
getReductionCount = fmap evalEnv'reductions get

bumpReductionCount :: Eval ()
bumpReductionCount = modify' $ \st -> st { evalEnv'reductions = succ $ evalEnv'reductions st }

evalExpr :: InputEnv -> LEnv -> ExprCore -> Eval Val
evalExpr inpEnv = recur
  where
    evalVar lenv x
      | Just v <- x `Map.lookup` lenv = pure v
      | otherwise = throwError $ EvalErr $ "Unknown variable: " ++ show x

    recur :: LEnv -> ExprCore -> Eval Val
    recur lenv expr = do
      bumpReductionCount
      evalCount <- getReductionCount
      if evalCount > Const.evalReductionLimit
        then throwError "Exceeds evaluation limit"
        else
          case expr of
            EVar     x   -> evalVar lenv x
            EPrim p      -> pure $ ValP p
            EPrimOp op   -> evalPrimOp inpEnv op
            EAp f x -> do
              valF :: (Val -> Eval Val) <- match =<< recur lenv f
              valX <- recur lenv x
              fmap inj $ valF valX
            ELam nm _ body -> pure $ ValF $ \x -> recur (Map.insert nm x lenv) body
            EIf e a b -> do
              valCond <- recur lenv e
              case valCond of
                ValP (PrimBool f) -> recur lenv $ if f then a else b
                _                 -> throwError TypeMismatch
            ELet nm bind body -> do
              valBind <- recur lenv bind
              let lenv' = MapL.insert nm valBind lenv
              recur lenv' body
            --
            ECase e alts -> do
              valE <- recur lenv e
              case valE of
                ValCon tag fields -> matchCase alts
                  where
                    matchCase (CaseAlt{..} : cs)
                      | tag == caseAlt'tag = recur (bindParams fields caseAlt'args lenv) caseAlt'rhs
                      | otherwise          = matchCase cs
                    matchCase [] = throwError $ EvalErr "No match in case"
                    --
                    bindParams []     []     = id
                    bindParams (v:vs) (n:ns) = bindParams vs ns . Map.insert n v
                    bindParams _      _      = error "Type error in case"
                _             -> throwError TypeMismatch
            EConstr (TupleT ts) 0 -> pure $ constr 0 (length ts)
            EConstr (ListT  _ ) 0 -> pure $ constr 0 0
            EConstr (ListT  _ ) 1 -> pure $ constr 1 2
            EConstr _           _ -> throwError "Invalid constructor"
            --
            EBottom{} -> throwError "Bottom encountered"

-- Generate constructor
constr :: Int -> Int -> Val
constr tag arity = build
  (\v xs -> xs . (v:))
  (\f    -> ValCon tag (f []))
  id
  arity

build :: (Val -> a -> a) -> (a -> Val) -> a -> Int -> Val
build step fini = go
  where
    go a 0 = fini a
    go a i = ValF $ \v -> pure $ go (step v a) (i-1)


----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

evalPrimOp :: InputEnv -> PrimOp TypeCore -> Eval Val
evalPrimOp env = \case
  OpAdd -> pure $ lift2 ((+) @Int64)
  OpSub -> pure $ lift2 ((-) @Int64)
  OpMul -> pure $ lift2 ((*) @Int64)
  OpDiv -> pure $ lift2 (div @Int64)
  OpNeg -> pure $ lift1 (negate @Int64)
  --
  OpBoolAnd -> pure $ lift2 (&&)
  OpBoolOr  -> pure $ lift2 (||)
  OpBoolXor -> pure $ lift2 (xor @Bool)
  OpBoolNot -> pure $ lift1 not
  --
  OpSigBool -> pure $ lift1 $ Fix . SigmaBool
  OpSigAnd  -> pure $ lift2 $ \a b -> Fix $ SigmaAnd [a,b]
  OpSigOr   -> pure $ lift2 $ \a b -> Fix $ SigmaOr  [a,b]
  OpSigPK   -> pure $ evalLift1 $ \t   -> fmap (Fix . SigmaPk) $ parsePublicKey t
  OpSigListAnd   -> pure $ lift1 $ Fix . SigmaAnd
  OpSigListOr    -> pure $ lift1 $ Fix . SigmaOr
  OpSigListAll _ -> pure $ Val2F $ \valF valXS -> fmap inj $ do
    f  <- match @(Val -> Eval Val) valF
    xs <- match @[Val]        valXS
    fmap (Fix . SigmaAnd) $ mapM (match <=< f) xs
  OpSigListAny _ -> pure $ Val2F $ \valF valXS -> fmap inj $ do
    f  <- match @(Val -> Eval Val) valF
    xs <- match @[Val]        valXS
    fmap (Fix . SigmaAnd) $ mapM (match <=< f) xs
  --
  OpCheckSig -> pure $ evalLift2 $ \bs sigIndex -> do
    pk  <- parsePublicKey bs
    sig <- readSig env sigIndex
    pure $ Crypto.verify pk sig (inputEnv'sigMsg env)
  OpCheckMultiSig -> pure $ evalLift3 $ \(total :: Int64) keysBS sigIndices -> do
      keys <- mapM parsePublicKey keysBS
      sigs <- mapM (readSig env) sigIndices
      let sigCount = sum $ zipWith (\key sig -> bool 0 1 (Crypto.verify key sig msg)) keys sigs
          msg = inputEnv'sigMsg env
      return $ sigCount >= total
  --
  OpEQ _ -> pure $ opComparison (==)
  OpNE _ -> pure $ opComparison (/=)
  OpLT _ -> pure $ opComparison (<)
  OpLE _ -> pure $ opComparison (<=)
  OpGT _ -> pure $ opComparison (>)
  OpGE _ -> pure $ opComparison (>=)
  --
  OpTextLength  -> pure $ lift1 (fromIntegral @_ @Int64 . T.length)
  OpTextAppend  -> pure $ lift2 ((<>) @Text)
  OpBytesLength -> pure $ lift1 (fromIntegral @_ @Int64 . BS.length)
  OpBytesAppend -> pure $ lift2 ((<>) @ByteString)
  OpSHA256      -> pure $ lift1 (hashBlob @SHA256)
  --
  OpShow t
    | t == IntT  -> pure $ lift1 (T.pack . show @Int64)
    | t == BoolT -> pure $ lift1 (T.pack . show @Bool)
    | otherwise  -> throwError "Invalid show"
  --
  OpToBytes   tag -> pure $ case tag of
    IntArg   -> lift1 $ serialise @Int64
    TextArg  -> lift1 $ serialise @Text
    BoolArg  -> lift1 $ serialise @Bool
    BytesArg -> lift1 $ serialise @ByteString
  OpFromBytes tag -> pure $ case tag of
    IntArg   -> evalLift1 $ decode @Int64
    TextArg  -> evalLift1 $ decode @Text
    BoolArg  -> evalLift1 $ decode @Bool
    BytesArg -> evalLift1 $ decode @ByteString
  --
  OpArgs tag -> pure $ case tag of
    IntArg   -> inj args'ints
    TextArg  -> inj args'texts
    BoolArg  -> inj args'bools
    BytesArg -> inj args'bytes
    where
      Args{..} = inputEnv'args env
  OpGetBoxId -> pure $ evalLift1 $ \case
    ValCon 0 [b,_,_,_,_] -> pure $ b
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxScript -> pure $ evalLift1 $ \case
    ValCon 0 [_,b,_,_,_] -> pure $ b
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxValue -> pure $ evalLift1 $ \case
    ValCon 0 [_,_,i,_,_] -> pure $ i
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxArgs t -> pure $ ValF $ \x -> case x of
    ValCon 0 [_,_,_, ValCon 0 [ints, txts, bools, bytes],_] -> case t of
      IntArg   -> pure $ ints
      TextArg  -> pure $ txts
      BoolArg  -> pure $ bools
      BytesArg -> pure $ bytes
    p -> throwError $ EvalErr $ "Not a box. Got " ++ show p
  OpGetBoxPostHeight -> pure $ evalLift1 $ \case
    ValCon 0 [_,_,_,_,b] -> pure $ b
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  --
  OpEnvGetHeight  -> pure $ ValP $ PrimInt $ inputEnv'height env
  OpEnvGetSelf    -> pure $ inj $ inputEnv'self env
  OpEnvGetInputs  -> pure $ inj $ inputEnv'inputs  env
  OpEnvGetOutputs -> pure $ inj $ inputEnv'outputs env
  --
  OpListMap _ _  -> pure $ evalLift2 (mapM :: (Val -> Eval Val) -> [Val] -> Eval [Val])
  OpListAt  _    -> pure $ evalLift2 lookAt
  OpListAppend _ -> pure $ lift2 ((<>) @[Val])
  OpListLength _ -> pure $ lift1 (fromIntegral @_ @Int64 . length @[] @Val)
  OpListFoldr{}  -> pure $ ValF $ \valF -> pure $ ValF $ \valZ -> pure $ ValF $ \valXS -> fmap inj $ do
    xs <- match @[Val] valXS
    f1 <- match @(Val -> Eval Val) valF
    let step :: Val -> Val -> Eval Val
        step a b = do
          f2 <- match =<< f1 a
          f2 b
    foldrM step valZ xs
  OpListFoldl{}  -> pure $ ValF $ \valF -> pure $ ValF $ \valZ -> pure $ ValF $ \valXS -> fmap inj $ do
    xs <- match @[Val] valXS
    f  <- match @(Val -> Eval Val) valF
    let step :: Val -> Val -> Eval Val
        step a b = do
          f2 <- match =<< f a
          f2 b
    foldM step valZ xs
  OpListFilter _ -> pure $ Val2F $ \valF valXS -> fmap inj $ do
    xs <- match @[Val]        valXS
    p  <- match @(Val -> Eval Val) valF
    filterM (match <=< p) xs
  OpListSum   -> pure $ lift1 (sum @[] @Int64)
  OpListAnd   -> pure $ lift1 (and @[])
  OpListOr    -> pure $ lift1 (or  @[])
  OpListAll _ -> pure $ Val2F $ \valF valXS -> do
    f  <- match @(Val -> Eval Val) valF
    xs <- match @[Val] valXS
    let step []       = pure $ inj True
        step (a : as) = do
          resBool <- match =<< f a
          case resBool of
            True  -> step as
            False -> pure $ inj False
    step xs
  OpListAny _ -> pure $ Val2F $ \valF valXS -> do
    f  <- match @(Val -> Eval Val) valF
    xs <- match @[Val] valXS
    let step []       = pure $ inj False
        step (a : as) = do
          resBool <- match =<< f a
          case resBool of
            True  -> pure $ inj True
            False -> step as
    step xs
  where
    decode :: Serialise a => LB.ByteString -> Eval a
    decode bs = case deserialiseOrFail bs of
      Right a -> pure a
      Left  _ -> throwError $ EvalErr "Deserialize failed"
    --
    lookAt :: [Val] -> Int64 -> Eval Val
    lookAt []    !_ = throwError "Runtime error: lookAt"
    lookAt (x:_)  0 = pure x
    lookAt (_:xs) n = lookAt xs (n-1)

opComparison :: (forall a. Ord a => a -> a -> Bool) -> Val
opComparison (#) = evalLift2 go
  where
    go (PrimInt   a) (PrimInt   b) = pure $ ValP $ PrimBool $ a # b
    go (PrimBool  a) (PrimBool  b) = pure $ ValP $ PrimBool $ a # b
    go (PrimText  a) (PrimText  b) = pure $ ValP $ PrimBool $ a # b
    go (PrimBytes a) (PrimBytes b) = pure $ ValP $ PrimBool $ a # b
    -- FIXME: Comparison for sigma expressions?
    go (PrimSigma _) (PrimSigma _) = throwError TypeMismatch
    go  _             _            = throwError TypeMismatch


parsePublicKey :: ByteString -> Eval PublicKey
parsePublicKey = maybe err pure . decodeFromBS
  where
    err = throwError "Can't parse public key"

readSig :: InputEnv -> Int64 -> Eval Crypto.Signature
readSig InputEnv{..} index = maybe err pure (inputEnv'sigs V.!? fromIntegral index)
  where
    err = throwError "Index of signature is out of bound"

----------------------------------------------------------------
-- Lifting of functions
----------------------------------------------------------------

class MatchPrim a where
  match :: Val -> Eval a

class InjPrim a where
  inj :: a -> Val

instance MatchPrim Val where
  match = pure
instance MatchPrim Prim where
  match (ValP p) = pure p
  match _        = throwError "Expecting primitive"
instance MatchPrim Int64 where
  match (ValP (PrimInt a)) = pure a
  match _                  = throwError "Expecting Int"
instance MatchPrim Bool where
  match (ValP (PrimBool a)) = pure a
  match _                   = throwError "Expecting Bool"
instance MatchPrim Text where
  match (ValP (PrimText a))  = pure a
  match _                    = throwError "Expecting Text"
instance MatchPrim ByteString where
  match (ValP (PrimBytes a)) = pure a
  match _                    = throwError "Expecting Bytes"
instance MatchPrim LB.ByteString where
  match (ValP (PrimBytes a)) = pure $ LB.fromStrict a
  match _                    = throwError "Expecting Bytes"

instance k ~ PublicKey => MatchPrim (Sigma k) where
  match (ValP (PrimSigma a)) = pure a
  match _                    = throwError "Expecting Sigma"

instance MatchPrim (Val -> Eval Val) where
  match = \case
    ValF      f -> pure f
    Val2F     f -> pure $ pure . ValF . f
    Val3F     f -> pure $ pure . Val2F . f
    v           -> throwError $ EvalErr $ "Expecting function, got " ++ conName v

instance (Typeable a, MatchPrim a) => MatchPrim [a] where
  match (ValCon 0 [])     = pure []
  match (ValCon 1 [x,xs]) = liftA2 (:) (match x) (match xs)
  match p = throwError $ EvalErr $ "Expecting list of " ++ show (typeRep (Proxy @a)) ++ " got " ++ show p

instance InjPrim Val           where inj = id
instance InjPrim Int64         where inj = ValP . PrimInt
instance InjPrim Bool          where inj = ValP . PrimBool
instance InjPrim Text          where inj = ValP . PrimText
instance InjPrim ByteString    where inj = ValP . PrimBytes
instance InjPrim LB.ByteString where inj = inj . LB.toStrict
instance InjPrim (Hash a)      where inj (Hash h) = inj h

instance k ~ PublicKey => InjPrim (Sigma k) where
  inj = ValP . PrimSigma

instance InjPrim a => InjPrim [a] where
  inj []     = ValCon 0 []
  inj (x:xs) = ValCon 1 [ inj x, inj xs ]

instance InjPrim a => InjPrim (V.Vector a) where
  inj = inj . V.toList

instance InjPrim BoxId where
  inj (BoxId a) = inj a

instance InjPrim (BoxId, PostBox) where
  inj (boxId, PostBox{..}) = ValCon 0
    [ inj boxId
    , inj $ unScript $ box'script postBox'content
    , inj $ box'value postBox'content
    , inj $ box'args postBox'content
    , inj postBox'height
    ]

-- | As a convenience we treat BoxInput as simply box
instance InjPrim BoxInput where
  inj BoxInput{..} = inj (boxInput'id, boxInput'box)

-- | As a convenience we treat BoxInput as simply box
instance InjPrim BoxOutput where
  inj BoxOutput{..} = inj (boxOutput'id, boxOutput'box)

instance InjPrim Args where
  inj Args{..} = ValCon 0
    [ inj args'ints
    , inj args'texts
    , inj args'bools
    , inj args'bytes
    ]

lift1 :: (MatchPrim a, InjPrim b) => (a -> b) -> Val
lift1 f = ValF $ go
  where
    go a = inj . f <$> match a

evalLift1 :: (MatchPrim a, InjPrim b) => (a -> Eval b) -> Val
evalLift1 f = ValF $ go
  where
    go a = fmap inj $ f =<< match a

lift2 :: (MatchPrim a, MatchPrim b, InjPrim c) => (a -> b -> c) -> Val
lift2 f = Val2F $ \a b -> go a b
  where
    go a b = fmap inj $ f <$> match a <*> match b

evalLift2 :: (MatchPrim a, MatchPrim b, InjPrim c) => (a -> b -> Eval c) -> Val
evalLift2 f = Val2F $ \a b -> go a b
  where
    go a b = fmap inj $ join $ f <$> match a <*> match b

evalLift3 :: (MatchPrim a, MatchPrim b, MatchPrim c, InjPrim d) => (a -> b -> c -> Eval d) -> Val
evalLift3 f = Val3F $ \a b c -> go a b c
  where
    go a b c = fmap inj $ join $ f <$> match a <*> match b <*>  match c

conName :: Val -> String
conName = \case
  ValP p      -> "Primitive: " ++ show p
  ValF{}      -> "ValF"
  Val2F{}     -> "Val2F"
  Val3F{}     -> "Val3F"
  ValCon{}    -> "ValCon"


{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE MultiWayIf          #-}
{-# LANGUAGE RankNTypes          #-}
-- | Evaluator for Core
module Hschain.Utxo.Lang.Core.RefEval
  ( Val(..)
  , EvalErr(..)
  , evalProg
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Applicative
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Int
import Data.ByteString (ByteString)
import Data.Bool
import Data.Text       (Text)
import Data.Typeable
import Data.Fix
import Data.Foldable (foldrM)
import Data.Vector.Generic ((!?))
import qualified Data.Vector          as V
import qualified Data.Text            as T
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LB

import HSChain.Crypto     (Hash(..),hashBlob,ByteRepr(..))
import HSChain.Crypto.SHA (SHA256)
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Types (Box(..),PostBox(..),BoxOutput(..),BoxInput(..),Args(..)
                               ,Script(..),BoxId(..),InputEnv(..))

import qualified Hschain.Utxo.Lang.Const as Const
import qualified Hschain.Utxo.Lang.Crypto.Signature as Crypto

-- | Value hanled by evaluator
data Val
  = ValP !Prim                            -- ^ Primitive value
  | ValF  (Val -> Eval Val)               -- ^ Unary function
  | Val2F (Val -> Val -> Eval Val)        -- ^ Binary function. Added in order to make defining primops easier
  | Val3F (Val -> Val -> Val -> Eval Val) -- ^ Ternary function. Added in order to make defining primops easier
  | ValCon (PrimCon TypeCore) [Val]       -- ^ Constructor cell

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

-- | Evaluate program
evalProg :: InputEnv -> Core v -> Either EvalErr TermVal
evalProg env prog = fromVal =<< runEval (evalExpr env [] prog)
  where
    fromVal = \case
      ValP p        -> Right $ PrimVal p
      ValF{}        -> Left $ EvalErr "Returning function"
      Val2F{}       -> Left $ EvalErr "Returning function"
      Val3F{}       -> Left $ EvalErr "Returning function"
      ValCon con xs -> fmap (ConVal con . V.fromList) $ mapM fromVal xs

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

evalExpr :: InputEnv -> [Val] -> Core v -> Eval Val
evalExpr inpEnv = recur
  where
    evalVar lenv x
      | Just v <- lookupVar lenv x = pure v
      | otherwise = throwError $ EvalErr "Unknown variable"
    --
    recur lenv expr = do
      bumpReductionCount
      evalCount <- getReductionCount
      if evalCount > Const.evalReductionLimit
        then throwError "Exceeds evaluation limit"
        else
          case expr of
            EVar _     -> throwError "Free variable"
            BVar i     -> evalVar lenv i
            EPrim p    -> pure $ ValP p
            EPrimOp op -> evalPrimOp inpEnv op
            EAp f x -> do
              valF :: (Val -> Eval Val) <- match =<< recur lenv f
              valX <- recur lenv x
              fmap inj $ valF valX
            ELam _ body -> pure $ ValF $ \x -> recur (x : lenv) body
            EIf e a b -> do
              valCond <- recur lenv e
              case valCond of
                ValP (PrimBool f) -> recur lenv $ if f then a else b
                _                 -> throwError TypeMismatch
            ELet bind body -> do x <- recur lenv bind
                                 recur (x : lenv) body
            --
            ECase e alts -> do
              valE <- recur lenv e
              case valE of
                ValCon tag fields -> matchCase alts
                  where
                    matchCase (CaseAlt{..} : cs)
                      | tag == caseAlt'tag = if
                        | length fields == caseAlt'nVars -> recur (fields <> lenv) caseAlt'rhs
                        | otherwise                      -> throwError TypeMismatch
                      | otherwise          = matchCase cs
                    matchCase [] = throwError $ EvalErr $ "No match in case with " <> (show tag)

                _             -> throwError TypeMismatch
            EConstr con  -> pure $ constr con
            --
            EBottom{} -> throwError "Bottom encountered"

-- Generate constructor
constr :: PrimCon TypeCore -> Val
constr tag = build
  (\v xs -> xs . (v:))
  (\f    -> ValCon tag (f []))
  id
  (conArity tag)

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
  OpDiv -> pure $ ValF $ \x -> pure $ ValF $ \y -> do
    valY <- match @Int64 y
    if valY == 0
      then throwError "divide by zero"
      else do
        valX <- match x
        pure $ inj $ div valX valY
  OpNeg -> pure $ lift1 (negate @Int64)
  --
  OpBoolAnd -> pure $ ValF $ \x -> pure $ ValF $ \y -> do
    valX <- match x
    case valX of
      True  -> match y
      False -> pure $ inj False
  OpBoolOr  -> pure $ ValF $ \x -> pure $ ValF $ \y -> do
    valX <- match x
    case valX of
      True  -> pure $ inj True
      False -> match y
  OpBoolNot -> pure $ lift1 not
  --
  OpSigBool   -> pure $ lift1 $ Fix . SigmaBool
  OpSigAnd    -> pure $ lift2 $ \a b -> Fix $ SigmaAnd [a,b]
  OpSigOr     -> pure $ lift2 $ \a b -> Fix $ SigmaOr  [a,b]
  OpSigPK     -> pure $ evalLift1 $ \t -> fmap (Fix . SigmaPk . dlogInput) $ parsePublicKey t
  OpSigDTuple -> pure $ evalLift3 $ \genB keyA keyB -> liftA3 (\gB pkA pkB -> Fix $ SigmaPk $ dtupleInput gB pkA pkB) (parseGenerator genB) (parsePublicKey keyA) (parsePublicKey keyB)
  OpSigListAnd   -> pure $ lift1 $ Fix . SigmaAnd
  OpSigListOr    -> pure $ lift1 $ Fix . SigmaOr
  OpSigListAll _ -> pure $ Val2F $ \valF valXS -> fmap inj $ do
    f  <- match @(Val -> Eval Val) valF
    xs <- match @[Val]        valXS
    fmap (Fix . SigmaAnd) $ mapM (match <=< f) xs
  OpSigListAny _ -> pure $ Val2F $ \valF valXS -> fmap inj $ do
    f  <- match @(Val -> Eval Val) valF
    xs <- match @[Val]        valXS
    fmap (Fix . SigmaOr) $ mapM (match <=< f) xs
  --
  OpCheckSig -> pure $ evalLift2 $ \bs sigIndex -> do
    pk  <- parsePublicKey bs
    sig <- readSig env sigIndex
    pure $ Crypto.verify pk sig $ boxInput'sigMsg $ inputEnv'self env
  OpCheckMultiSig -> pure $ evalLift3 $ \(total :: Int64) keysBS sigIndices -> do
      keys <- mapM parsePublicKey keysBS
      sigs <- mapM (readSig env) sigIndices
      let sigCount = sum $ zipWith (\key sig -> bool 0 1 (Crypto.verify key sig msg)) keys sigs
          msg = boxInput'sigMsg $ inputEnv'self env
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
  OpShow _ -> pure $ liftTerm1 (PrimVal . PrimText . renderText)
  --
  OpToBytes   _ -> pure $ liftTerm1 (PrimVal . PrimBytes . LB.toStrict . serialise)
  OpFromBytes _  -> let getBS = \case
                         PrimVal (PrimBytes bs) -> pure $ LB.fromStrict bs
                         _                      -> throwError "Not a bytestring"
                   in  pure $ evalLiftTerm1 $ (decode <=< getBS)
  --
  OpArgs _ -> fmap injTerm $ decode $ LB.fromStrict bs
    where
      Args bs = boxInput'args $ inputEnv'self env

  OpGetBoxId -> pure $ evalLift1 $ \case

    ValCon _ [b,_,_,_,_] -> pure $ b
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxScript -> pure $ evalLift1 $ \case
    ValCon _ [_,b,_,_,_] -> pure $ b
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxValue -> pure $ evalLift1 $ \case
    ValCon _ [_,_,i,_,_] -> pure $ i
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxArgs _ -> pure $ ValF $ \x -> case x of
    ValCon _ [_,_,_, ValP (PrimBytes bs), _] -> fmap injTerm $ decode $ LB.fromStrict bs
    p -> throwError $ EvalErr $ "Not a box. Got " ++ show p
  OpGetBoxPostHeight -> pure $ evalLift1 $ \case
    ValCon _ [_,_,_,_,b] -> pure $ b
    x                    -> throwError $ EvalErr $ "Box expected, got" ++ show x
  --
  OpEnvGetHeight     -> pure $ ValP $ PrimInt $ inputEnv'height env
  OpEnvGetSelf       -> pure $ inj $ inputEnv'self env
  OpEnvGetInputs     -> pure $ inj $ inputEnv'inputs  env
  OpEnvGetOutputs    -> pure $ inj $ inputEnv'outputs env
  OpEnvGetDataInputs -> pure $ inj $ inputEnv'dataInputs env
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
  OpListSum     -> pure $ lift1 (sum @[] @Int64)
  OpListProduct -> pure $ lift1 (product @[] @Int64)
  OpListAnd     -> pure $ ValF $ \valXS -> do
    xs <- match @[Val] valXS
    let step []       = pure $ inj True
        step (a : as) = do
          resBool <- match a
          case resBool of
            True  -> step as
            False -> pure $ inj False
    step xs
  OpListOr    -> pure $ ValF $ \valXS -> do
    xs <- match @[Val] valXS
    let step []       = pure $ inj False
        step (a : as) = do
          resBool <- match a
          case resBool of
            True  -> pure $ inj True
            False -> step as
    step xs
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

opComparison :: (TermVal -> TermVal -> Bool) -> Val
opComparison f = liftTerm2 (\a b -> PrimVal $ PrimBool $ f a b)

parsePublicKey :: ByteString -> Eval PublicKey
parsePublicKey = parseBS err
  where
    err = "Can't parse public key"

parseGenerator :: ByteString -> Eval ECPoint
parseGenerator = parseBS err
  where
    err = "Can't parse ECPoint"

parseBS :: ByteRepr a => String -> ByteString -> Eval a
parseBS msg = maybe (throwError $ EvalErr msg) pure . decodeFromBS

readSig :: InputEnv -> Int64 -> Eval Crypto.Signature
readSig InputEnv{..} index =
  case boxInput'sigs inputEnv'self !? fromIntegral index of
    Nothing -> throwError "Index of signature is out of bound"
    Just v  -> pure v

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

instance k ~ ProofInput => MatchPrim (Sigma k) where
  match (ValP (PrimSigma a)) = pure a
  match _                    = throwError "Expecting Sigma"

instance MatchPrim (Val -> Eval Val) where
  match = \case
    ValF      f -> pure f
    Val2F     f -> pure $ pure . ValF . f
    Val3F     f -> pure $ pure . Val2F . f
    v           -> throwError $ EvalErr $ "Expecting function, got " ++ toName v
    where
      toName :: Val -> String
      toName = \case
        ValP p      -> "Primitive: " ++ show p
        ValF{}      -> "ValF"
        Val2F{}     -> "Val2F"
        Val3F{}     -> "Val3F"
        ValCon{}    -> "ValCon"

instance (Typeable a, MatchPrim a) => MatchPrim [a] where
  match (ValCon (ConNil _)  [])     = pure []
  match (ValCon (ConCons _) [x,xs]) = liftA2 (:) (match x) (match xs)
  match p = throwError $ EvalErr $ "Expecting list of " ++ show (typeRep (Proxy @a)) ++ " got " ++ show p

instance InjPrim Val           where inj = id
instance InjPrim Int64         where inj = ValP . PrimInt
instance InjPrim Bool          where inj = ValP . PrimBool
instance InjPrim Text          where inj = ValP . PrimText
instance InjPrim ByteString    where inj = ValP . PrimBytes
instance InjPrim LB.ByteString where inj = inj . LB.toStrict
instance InjPrim (Hash a)      where inj (Hash h) = inj h

instance k ~ ProofInput => InjPrim (Sigma k) where
  inj = ValP . PrimSigma

instance InjPrim a => InjPrim [a] where
  inj []     = ValCon (ConNil UnitT)  []
  inj (x:xs) = ValCon (ConCons UnitT) [ inj x, inj xs ]

instance InjPrim a => InjPrim (V.Vector a) where
  inj = inj . V.toList

instance InjPrim BoxId where
  inj (BoxId a) = inj a

instance InjPrim (BoxId, PostBox) where
  inj (boxId, PostBox{..}) = ValCon boxPrimCon
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
  inj (Args bs) = inj bs

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

-------------------------------------------
-- match terms

matchTerm :: Val -> Eval TermVal
matchTerm = \case
  ValP p          -> pure $ PrimVal p
  ValCon con args -> fmap (ConVal con . V.fromList) $ mapM matchTerm args
  _               -> throwError "It is not a term"

injTerm :: TermVal -> Val
injTerm = \case
  PrimVal p       -> ValP p
  ConVal con args -> ValCon con $ V.toList $ fmap injTerm args

liftTerm1 :: (TermVal -> TermVal) -> Val
liftTerm1 f = ValF $ go
  where
    go a = injTerm . f <$> matchTerm a

liftTerm2 :: (TermVal -> TermVal -> TermVal) -> Val
liftTerm2 f = Val2F $ \a b -> go a b
  where
    go a b = fmap injTerm $ f <$> matchTerm a <*> matchTerm b

evalLiftTerm1 :: (TermVal -> Eval TermVal) -> Val
evalLiftTerm1 f = ValF $ go
  where
    go a = fmap injTerm $ f =<< matchTerm a


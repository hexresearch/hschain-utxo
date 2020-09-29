{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes          #-}
-- |
module Hschain.Utxo.Lang.Core.RefEval
  ( Val(..)
  , EvalErr(..)
  , EvalResult(..)
  , evalProg
  ) where

import Codec.Serialise (Serialise,serialise,deserialiseOrFail)
import Control.Applicative
import Control.Monad
import Data.Int
import Data.Bits       (xor)
import Data.ByteString (ByteString)
import Data.Text       (Text)
import Data.Typeable
import Data.Fix
import qualified Data.Vector     as V
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy   as MapL
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LB

import HSChain.Crypto     (Hash(..),hashBlob)
import HSChain.Crypto.SHA (SHA256)
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Expr  (ArgType(..), Box(..), Args(..), Script(..), BoxId(..))
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (InputEnv(..))

-- | Value hanled by evaluator
data Val
  = ValP !Prim                  -- ^ Primitive value
  | ValBottom !EvalErr          -- ^ Bottom. Always terminate evaluation
  | ValF  (Val -> Val)          -- ^ Unary function
  | Val2F (Val -> Val -> Val)   -- ^ Binary function. Added in order to make defining primops easier
  | ValCon Int [Val]            -- ^ Constructor cell

instance Show Val where
  showsPrec n v
    = showParen (n >= 11)
    $ case v of
        ValP      p -> showsPrec 11 p
        ValBottom e -> showsPrec 11 e
        ValF  _     -> showString "Function"
        Val2F _     -> showString "Function"
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

-- | Local evaluation environment. Map from variable bindings to values
type LEnv = Map.Map Name Val

-- | Evaluate program
evalProg :: InputEnv -> ExprCore -> EvalResult
evalProg env prog =
  case evalExpr env mempty prog of
    ValP p      -> EvalPrim p
    ValBottom e -> EvalFail e
    ValF{}      -> EvalFail $ EvalErr "Returning function"
    Val2F{}     -> EvalFail $ EvalErr "Returning function"
    ValCon i xs -> maybe (EvalFail $ EvalErr "Not a list") EvalList
                 $ con2list i xs
  where
    con2list 0 []                   = Just []
    con2list 1 [ValP p,ValCon i xs] = (p :) <$> con2list i xs
    con2list _ _                    = Nothing

evalExpr :: InputEnv -> LEnv -> ExprCore -> Val
evalExpr inpEnv = recur
  where
    evalVar lenv x
      | Just v <- x `Map.lookup` lenv = v
      | otherwise = ValBottom $ EvalErr $ "Unknown variable: " ++ show x
    recur lenv = \case
      EVar     x   -> evalVar lenv x
      EPrim p      -> ValP p
      EPrimOp op   -> evalPrimOp inpEnv op
      EAp f x -> inj $ do
        valF <- match $ recur lenv f
        return (valF $ recur lenv x :: Val)
      ELam nm _ body -> ValF $ \x -> recur (Map.insert nm x lenv) body
      EIf e a b -> case recur lenv e of
        ValP (PrimBool f) -> recur lenv $ if f then a else b
        ValBottom err     -> ValBottom err
        _                 -> ValBottom TypeMismatch
      ELet nm bind body ->
        let lenv' = MapL.insert nm (recur lenv bind) lenv
        in recur lenv' body
      --
      ECase e alts -> case recur lenv e of
        ValCon tag fields -> matchCase alts
          where
            matchCase (CaseAlt{..} : cs)
              | tag == caseAlt'tag = recur (bindParams fields caseAlt'args lenv) caseAlt'rhs
              | otherwise          = matchCase cs
            matchCase [] = ValBottom $ EvalErr "No match in case"
            --
            bindParams []     []     = id
            bindParams (v:vs) (n:ns) = bindParams vs ns . Map.insert n v
            bindParams _      _      = error "Type error in case"
        ValBottom err -> ValBottom err
        _             -> ValBottom TypeMismatch
      EConstr (TupleT ts) 0 -> constr 0 (length ts)
      EConstr (ListT  _ ) 0 -> constr 0 0
      EConstr (ListT  _ ) 1 -> constr 1 2
      EConstr _           _ -> ValBottom $ EvalErr "Invalid constructor"
      --
      EBottom{} -> ValBottom $ EvalErr "Bottom encountered"

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
    go a i = ValF $ \v -> go (step v a) (i-1)


----------------------------------------------------------------
-- Primitives
----------------------------------------------------------------

evalPrimOp :: InputEnv -> PrimOp TypeCore -> Val
evalPrimOp env = \case
  OpAdd -> lift2 ((+) @Int64)
  OpSub -> lift2 ((-) @Int64)
  OpMul -> lift2 ((*) @Int64)
  OpDiv -> lift2 (div @Int64)
  OpNeg -> lift1 (negate @Int64)
  --
  OpBoolAnd -> lift2 (&&)
  OpBoolOr  -> lift2 (||)
  OpBoolXor -> lift2 (xor @Bool)
  OpBoolNot -> lift1 not
  --
  OpSigBool -> lift1 $ Fix . SigmaBool
  OpSigAnd  -> lift2 $ \a b -> Fix $ SigmaAnd [a,b]
  OpSigOr   -> lift2 $ \a b -> Fix $ SigmaOr  [a,b]
  OpSigPK   -> lift1 $ \t   -> case publicKeyFromText t of
                                 Nothing -> Left  $ EvalErr "Can't parse public key"
                                 Just k  -> Right $ Fix $ SigmaPk k
  OpSigListAnd   -> lift1 $ Fix . SigmaAnd
  OpSigListOr    -> lift1 $ Fix . SigmaOr
  OpSigListAll _ -> Val2F $ \valF valXS -> inj $ do
    f  <- match @(Val -> Val) valF
    xs <- match @[Val]        valXS
    Fix . SigmaAnd <$> mapM (match . f) xs
  OpSigListAny _ -> Val2F $ \valF valXS -> inj $ do
    f  <- match @(Val -> Val) valF
    xs <- match @[Val]        valXS
    Fix . SigmaOr <$> mapM (match . f) xs
  --
  OpEQ _ -> opComparison (==)
  OpNE _ -> opComparison (/=)
  OpLT _ -> opComparison (<)
  OpLE _ -> opComparison (<=)
  OpGT _ -> opComparison (>)
  OpGE _ -> opComparison (>=)
  --
  OpTextLength  -> lift1 (fromIntegral @_ @Int64 . T.length)
  OpTextAppend  -> lift2 ((<>) @Text)
  OpBytesLength -> lift1 (fromIntegral @_ @Int64 . BS.length)
  OpBytesAppend -> lift2 ((<>) @ByteString)
  OpSHA256      -> lift1 (hashBlob @SHA256)
  --
  OpShow t
    | t == IntT  -> lift1 (T.pack . show @Int64)
    | t == BoolT -> lift1 (T.pack . show @Bool)
    | otherwise  -> ValBottom $ EvalErr "Invalid show"
  --
  OpToBytes   tag -> case tag of
    IntArg   -> lift1 $ serialise @Int64
    TextArg  -> lift1 $ serialise @Text
    BoolArg  -> lift1 $ serialise @Bool
    BytesArg -> lift1 $ serialise @ByteString
  OpFromBytes tag -> case tag of
    IntArg   -> lift1 $ decode @Int64
    TextArg  -> lift1 $ decode @Text
    BoolArg  -> lift1 $ decode @Bool
    BytesArg -> lift1 $ decode @ByteString
  --
  OpArgs tag -> case tag of
    IntArg   -> inj args'ints
    TextArg  -> inj args'texts
    BoolArg  -> inj args'bools
    BytesArg -> inj args'bytes
    where
      Args{..} = inputEnv'args env
  OpGetBoxId -> lift1 $ \case
    ValCon 0 [b,_,_,_] -> b
    x                  -> ValBottom $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxScript -> lift1 $ \case
    ValCon 0 [_,b,_,_] -> b
    x                  -> ValBottom $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxValue -> lift1 $ \case
    ValCon 0 [_,_,i,_] -> i
    x                  -> ValBottom $ EvalErr $ "Box expected, got" ++ show x
  OpGetBoxArgs t -> ValF $ \case
    ValCon 0 [_,_,_, ValCon 0 [ints, txts, bools, bytes]] -> case t of
      IntArg   -> ints
      TextArg  -> txts
      BoolArg  -> bools
      BytesArg -> bytes
    p -> ValBottom $ EvalErr $ "Not a box. Got " ++ show p
  OpMakeBox -> Val2F $ \a b -> Val2F $ \c d -> ValCon 0 [a,b,c,d]
  --
  OpEnvGetHeight -> ValP $ PrimInt $ inputEnv'height env
  OpEnvGetSelf   -> inj $ inputEnv'self env
  OpEnvGetInputs  -> inj $ inputEnv'inputs  env
  OpEnvGetOutputs -> inj $ inputEnv'outputs env
  --
  OpListMap _ _  -> lift2 (fmap :: (Val -> Val) -> [Val] -> [Val])
  OpListAt  _    -> lift2 lookAt
  OpListAppend _ -> lift2 ((<>) @[Val])
  OpListLength _ -> lift1 (fromIntegral @_ @Int64 . length @[] @Val)
  OpListFoldr{}  -> ValF $ \valF -> ValF $ \valZ -> ValF $ \valXS -> inj $ do
    xs <- match @[Val] valXS
    f1 <- match @(Val -> Val) valF
    let step :: Val -> Val -> Val
        step a b = case match (f1 a) of
          Right f2 -> f2 b
          Left  e  -> ValBottom e
    return $ foldr step valZ xs
  OpListFoldl{}  -> ValF $ \valF -> ValF $ \valZ -> ValF $ \valXS -> inj $ do
    xs <- match @[Val] valXS
    f1 <- match @(Val -> Val) valF
    let step :: Val -> Val -> Val
        step a b = case match (f1 a) of
          Right f2 -> f2 b
          Left  e  -> ValBottom e
    return $ foldl step valZ xs
  OpListFilter _ -> Val2F $ \valF valXS -> inj $ do
    xs <- match @[Val]        valXS
    p  <- match @(Val -> Val) valF
    return $ filterM (match . p) xs
  OpListSum   -> lift1 (sum @[] @Int64)
  OpListAnd   -> lift1 (and @[])
  OpListOr    -> lift1 (or  @[])
  OpListAll _ -> Val2F $ \valF valXS -> inj $ do
    f  <- match @(Val -> Val) valF
    xs <- match @[Val] valXS
    let step []                 = inj True
        step (Right True  : as) = step as
        step (Right False : _ ) = inj False
        step (Left e      : _ ) = ValBottom e
    return $ step $ map (match . f) xs
  OpListAny _ -> Val2F $ \valF valXS -> inj $ do
    f  <- match @(Val -> Val) valF
    xs <- match @[Val] valXS
    let step []                 = inj False
        step (Right True  : _ ) = inj True
        step (Right False : as) = step as
        step (Left e      : _ ) = ValBottom e
    return $ step $ map (match . f) xs
  where
    decode :: Serialise a => LB.ByteString -> Either EvalErr a
    decode bs = case deserialiseOrFail bs of
      Right a -> Right a
      Left  _ -> Left $ EvalErr "Deserialize failed"
    --
    lookAt :: [Val] -> Int64 -> Val
    lookAt []    !_ = ValBottom $ EvalErr "Runtime error: lookAt"
    lookAt (x:_)  0 = x
    lookAt (_:xs) n = lookAt xs (n-1)

opComparison :: (forall a. Ord a => a -> a -> Bool) -> Val
opComparison (#) = lift2 go
  where
    go (PrimInt   a) (PrimInt   b) = ValP $ PrimBool $ a # b
    go (PrimBool  a) (PrimBool  b) = ValP $ PrimBool $ a # b
    go (PrimText  a) (PrimText  b) = ValP $ PrimBool $ a # b
    go (PrimBytes a) (PrimBytes b) = ValP $ PrimBool $ a # b
    -- FIXME: Comparison for sigma expressions?
    go (PrimSigma _) (PrimSigma _) = ValBottom TypeMismatch
    go  _             _            = ValBottom TypeMismatch


----------------------------------------------------------------
-- Lifting of functions
----------------------------------------------------------------

match :: MatchPrim a => Val -> Either EvalErr a
match (ValBottom e) = Left e
match v             = matchVal v

class MatchPrim a where
  matchVal :: Val -> Either EvalErr a

class InjPrim a where
  inj :: a -> Val

instance MatchPrim Val where
  matchVal = Right
instance MatchPrim Prim where
  matchVal (ValP p) = Right p
  matchVal _        = Left $ EvalErr "Expecting primitive"
instance MatchPrim Int64 where
  matchVal (ValP (PrimInt a)) = Right a
  matchVal _                  = Left $ EvalErr "Expecting Int"
instance MatchPrim Bool where
  matchVal (ValP (PrimBool a)) = Right a
  matchVal _                   = Left $ EvalErr "Expecting Bool"
instance MatchPrim Text where
  matchVal (ValP (PrimText a))  = Right a
  matchVal _                    = Left $ EvalErr "Expecting Text"
instance MatchPrim ByteString where
  matchVal (ValP (PrimBytes a)) = Right a
  matchVal _                    = Left $ EvalErr "Expecting Bytes"
instance MatchPrim LB.ByteString where
  matchVal (ValP (PrimBytes a)) = Right $ LB.fromStrict a
  matchVal _                    = Left $ EvalErr "Expecting Bytes"

instance k ~ PublicKey => MatchPrim (Sigma k) where
  matchVal (ValP (PrimSigma a)) = Right a
  matchVal _                    = Left $ EvalErr "Expecting Sigma"

instance MatchPrim (Val -> Val) where
  matchVal = \case
    ValF      f -> Right f
    Val2F     f -> Right $ ValF . f
    v           -> Left $ EvalErr $ "Expecting function, got " ++ conName v


instance (Typeable a, MatchPrim a) => MatchPrim [a] where
  matchVal (ValCon 0 [])     = Right []
  matchVal (ValCon 1 [x,xs]) = liftA2 (:) (match x) (match xs)
  matchVal p = Left $ EvalErr $ "Expecting list of " ++ show (typeRep (Proxy @a)) ++ " got " ++ show p

instance InjPrim Val           where inj = id
instance InjPrim Int64         where inj = ValP . PrimInt
instance InjPrim Bool          where inj = ValP . PrimBool
instance InjPrim Text          where inj = ValP . PrimText
instance InjPrim ByteString    where inj = ValP . PrimBytes
instance InjPrim LB.ByteString where inj = inj . LB.toStrict
instance InjPrim (Hash a)      where inj (Hash h) = inj h

instance k ~ PublicKey => InjPrim (Sigma k) where
  inj = ValP . PrimSigma

instance InjPrim a => InjPrim (Either EvalErr a) where
  inj = either ValBottom inj

instance InjPrim a => InjPrim [a] where
  inj []     = ValCon 0 []
  inj (x:xs) = ValCon 1 [ inj x, inj xs ]

instance InjPrim a => InjPrim (V.Vector a) where
  inj = inj . V.toList

instance InjPrim Box where
  inj Box{..} = ValCon 0
    [ inj $ unBoxId box'id
    , inj $ unScript box'script
    , inj box'value
    , inj box'args
    ]

instance InjPrim Args where
  inj Args{..} = ValCon 0
    [ inj args'ints
    , inj args'texts
    , inj args'bools
    , inj args'bytes
    ]


lift1 :: (MatchPrim a, InjPrim b) => (a -> b) -> Val
lift1 f = ValF go
  where
    go a = inj $ f <$> match a

lift2 :: (MatchPrim a, MatchPrim b, InjPrim c) => (a -> b -> c) -> Val
lift2 f = Val2F go
  where
    go a b = inj $ f <$> match a <*> match b


conName :: Val -> String
conName = \case
  ValP p      -> "Primitive: " ++ show p
  ValBottom e -> "Bottom: " ++ show e
  ValF{}      -> "ValF"
  Val2F{}     -> "Val2F"
  ValCon{}    -> "ValCon"

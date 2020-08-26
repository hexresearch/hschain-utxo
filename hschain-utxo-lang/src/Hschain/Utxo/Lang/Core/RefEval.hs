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
import Data.Int
import Data.Bits       (xor)
import Data.ByteString (ByteString)
import Data.Text       (Text)
import Data.Fix
import qualified Data.Vector     as V
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy   as MapL
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as LB

import HSChain.Crypto     (Hash(..),hashBlob)
import HSChain.Crypto.SHA (SHA256)
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (intT,boolT)
import Hschain.Utxo.Lang.Expr  (ArgType(..))
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (InputEnv(..))


-- | Value hanled by evaluator
data Val
  = ValP !Prim                  -- ^ Primitive value
  | ValBottom !EvalErr          -- ^ Bottom. Always terminate evaluation
  | ValF  (Val -> Val)          -- ^ Unary function
  | Val2F (Val -> Val -> Val)   -- ^ Binary function. Added in order to make defining primops easier
  | ValCon Int [Val]            -- ^ Constructor cell

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

-- | Global environment. Values for globally defined function.
type GEnv = Map.Map Name Val

-- | Loval evaluation environment. Map from supercombinator names to
--   their evaluation.
type LEnv = Map.Map Name Val


-- | Evaluate program
evalProg :: InputEnv -> CoreProg -> EvalResult
evalProg env (CoreProg prog) =
  case "main" `Map.lookup` genv of
    Nothing -> EvalFail $ EvalErr "No main function"
    Just v  -> case v of
      ValP p      -> EvalPrim p
      ValBottom e -> EvalFail e
      ValF{}      -> EvalFail $ EvalErr "Returning function"
      Val2F{}     -> EvalFail $ EvalErr "Returning function"
      ValCon i xs -> maybe (EvalFail $ EvalErr "Not a list") EvalList
                   $ con2list i xs
  where
    genv = MapL.fromList [ (scomb'name s, evalScomb env genv s)
                         | s <- prog ++ environmentFunctions env
                         ]
    --
    con2list 0 []                   = Just []
    con2list 1 [ValP p,ValCon i xs] = (p :) <$> con2list i xs
    con2list _ _                    = Nothing

evalScomb :: InputEnv -> GEnv -> Scomb -> Val
evalScomb inpEnv genv Scomb{..} = buildArg Map.empty (V.toList scomb'args)
  where
    buildArg e (x:xs) = ValF $ \a -> buildArg (Map.insert (typed'value x) a e) xs
    buildArg e []     = evalExpr inpEnv genv e $ typed'value scomb'body

evalExpr :: InputEnv -> GEnv -> LEnv -> ExprCore -> Val
evalExpr inpEnv genv = recur
  where
    evalVar lenv x
      | Just v <- x `Map.lookup` lenv          = v
      | Just v <- x `Map.lookup` genv          = v
      | Just v <- x `Map.lookup` primitivesMap = v
      | otherwise = ValBottom $ EvalErr $ "Unknown variable: " ++ show x
    recur lenv = \case
      EVar     x   -> evalVar lenv x
      EPolyVar x _ -> evalVar lenv x
      EPrim p      -> ValP p
      EPrimOp op   -> evalPrimOp inpEnv op
      EAp f x -> inj $ do
        valF <- matchP $ recur lenv f
        return (valF $ recur lenv x :: Val)
      EIf e a b -> case recur lenv e of
        ValP (PrimBool f) -> recur lenv $ if f then a else b
        ValBottom err     -> ValBottom err
        _                 -> ValBottom TypeMismatch
      -- FIXME: Here we assume that let is completely nonrecursive
      --        (For simplicity)
      ELet nm bind body ->
        let lenv' = MapL.insert nm (recur lenv bind) lenv
        in recur lenv' body
      --
      ECase e alts -> case recur lenv e of
        ValCon tag fields -> match alts
          where
            match (CaseAlt{..} : cs)
              | tag == caseAlt'tag = recur (bindParams fields caseAlt'args lenv) caseAlt'rhs
              | otherwise          = match cs
            match [] = ValBottom $ EvalErr "No match in case"
            --
            bindParams []             []             = id
            bindParams (v:vs) (Typed n _:ts) = bindParams vs ts . Map.insert n v
            bindParams _ _ = error "Type error in case"
        ValBottom err -> ValBottom err
        _             -> ValBottom TypeMismatch
      EConstr _ tag arity    -> constr tag arity
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

evalPrimOp :: InputEnv -> PrimOp -> Val
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
    | t == intT  -> lift1 (T.pack . show @Int64)
    | t == boolT -> lift1 (T.pack . show @Bool)
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
  OpEnvGetHeight -> ValP $ PrimInt $ inputEnv'height env
  OpListMap _ _  -> lift2 (fmap :: (Val -> Val) -> [Val] -> [Val])
  where
    decode :: Serialise a => LB.ByteString -> Either EvalErr a
    decode bs = case deserialiseOrFail bs of
      Right a -> Right a
      Left  _ -> Left $ EvalErr "Deserialize failed"

primitivesMap :: Map.Map Name Val
primitivesMap = MapL.fromList
  [ (scomb'name s, evalScomb dummyEnv mempty s) | s <- primitives ]

dummyEnv :: InputEnv
dummyEnv = error "Environment is inaccessible in the library functions"

opComparison :: (forall a. Ord a => a -> a -> Bool) -> Val
opComparison (#) = primFun2 go
  where
    go (PrimInt   a) (PrimInt   b) = ValP $ PrimBool $ a # b
    go (PrimBool  a) (PrimBool  b) = ValP $ PrimBool $ a # b
    go (PrimText  a) (PrimText  b) = ValP $ PrimBool $ a # b
    go (PrimBytes a) (PrimBytes b) = ValP $ PrimBool $ a # b
    -- FIXME: Comparison for sigma expressions?
    go (PrimSigma _) (PrimSigma _) = ValBottom TypeMismatch
    go _ _ = ValBottom TypeMismatch

primFun2 :: (Prim -> Prim -> Val) -> Val
primFun2 f = Val2F go
  where
    go (ValP a) (ValP b) = f a b
    go _        _        = ValBottom TypeMismatch


----------------------------------------------------------------
-- Lifting of functions
----------------------------------------------------------------

class MatchPrim a where
  matchP :: Val -> Either EvalErr a

class InjPrim a where
  inj :: a -> Val

instance MatchPrim Val where
  matchP = Right
instance MatchPrim Int64 where
  matchP (ValP (PrimInt a)) = Right a
  matchP _                  = Left $ EvalErr "Expecting Int"
instance MatchPrim Bool where
  matchP (ValP (PrimBool a)) = Right a
  matchP _                   = Left $ EvalErr "Expecting Bool"
instance MatchPrim Text where
  matchP (ValP (PrimText a))  = Right a
  matchP _                    = Left $ EvalErr "Expecting Text"
instance MatchPrim ByteString where
  matchP (ValP (PrimBytes a)) = Right a
  matchP _                    = Left $ EvalErr "Expecting Bytes"
instance MatchPrim LB.ByteString where
  matchP (ValP (PrimBytes a)) = Right $ LB.fromStrict a
  matchP _                    = Left $ EvalErr "Expecting Bytes"

instance k ~ PublicKey => MatchPrim (Sigma k) where
  matchP (ValP (PrimSigma a)) = Right a
  matchP _                    = Left $ EvalErr "Expecting Sigma"

instance MatchPrim (Val -> Val) where
  matchP = \case
    ValF  f -> Right f
    Val2F f -> Right $ ValF . f
    _       -> Left TypeMismatch

instance MatchPrim a => MatchPrim [a] where
  matchP (ValCon 0 [])     = Right []
  matchP (ValCon 1 [x,xs]) = liftA2 (:) (matchP x) (matchP xs)
  matchP _ = Left $ EvalErr "Expecting list"

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


lift1 :: (MatchPrim a, InjPrim b) => (a -> b) -> Val
lift1 f = ValF go
  where
    go a = inj $ f <$> matchP a

lift2 :: (MatchPrim a, MatchPrim b, InjPrim c) => (a -> b -> c) -> Val
lift2 f = Val2F go
  where
    go a b = inj $ f <$> matchP a <*> matchP b

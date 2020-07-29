{-# LANGUAGE MonadComprehensions #-}
{-# LANGUAGE RankNTypes          #-}
-- |
module Hschain.Utxo.Lang.Core.RefEval
  ( Val(..)
  , evalProg
  ) where

import Data.Int
import Data.ByteString (ByteString)
import Data.Text       (Text)
import Data.Fix
import qualified Data.Vector     as V
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map
import qualified Data.Map.Lazy   as MapL

import HSChain.Crypto     (Hash(..),hashBlob)
import HSChain.Crypto.SHA (SHA256)
import Hschain.Utxo.Lang.Core.Data.Prim
import Hschain.Utxo.Lang.Core.Data.Code
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.Primitives
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (InputEnv)


-- | Value hanled by evaluator
data Val
  = ValP !Prim                  -- ^ Primitive value
  | ValBottom                   -- ^ Bottom. Always terminate evaluation
  | ValF  (Val -> Val)          -- ^ Unary function
  | Val2F (Val -> Val -> Val)   -- ^ Binary function. Added in order to make defining primops easier
  | ValCon Int [Val]            -- ^ Constructor cell


-- | Global environment. Values for globally defined function.
type GEnv = Map.Map Name Val

-- | Loval evaluation environment. Map from supercombinator names to
--   their evaluation.
type LEnv = Map.Map Name Val


-- | Evaluate program
evalProg :: InputEnv -> CoreProg -> Maybe Prim
evalProg env (CoreProg prog) = do
  ValP p <- "main" `Map.lookup` genv
  return p
  where
    genv = MapL.fromList [ (scomb'name s, evalScomb genv s)
                         | s <- prog ++ environmentFunctions env
                         ]


evalScomb :: GEnv -> Scomb -> Val
evalScomb genv Scomb{..} = buildArg Map.empty (V.toList scomb'args)
  where
    buildArg e (x:xs) = ValF $ \a -> buildArg (Map.insert (typed'value x) a e) xs
    buildArg e []     = evalExpr genv e $ typed'value scomb'body

evalExpr :: GEnv -> LEnv -> ExprCore -> Val
evalExpr genv = recur
  where
    evalVar lenv x
      | Just v <- x `Map.lookup` lenv     = v
      | Just v <- x `Map.lookup` genv     = v
      | Just v <- x `Map.lookup` primVals = v
      | otherwise                         = ValBottom
    recur lenv = \case
      EVar     x   -> evalVar lenv x
      EPolyVar x _ -> evalVar lenv x
      EPrim p -> ValP p
      EAp f x -> case recur lenv f of
                   ValF  valF -> valF $ recur lenv x
                   Val2F valF -> ValF $ valF $ recur lenv x
                   _          -> ValBottom
      EIf e a b -> case recur lenv e of
        ValP (PrimBool f) -> recur lenv $ if f then a else b
        _                 -> ValBottom
      -- FIXME: Here we assume that let is completely nonrecursive
      --        (For simplicity)
      ELet binds body ->
        let lenv' = Map.fromList [ (nm, recur lenv e) | (nm,e) <- binds ]
                 <> lenv
        in recur lenv' body
      --
      ECase e alts -> case recur lenv e of
        ValCon tag fields -> match alts
          where
            match (CaseAlt{..} : cs)
              | tag == caseAlt'tag = recur (bindParams fields caseAlt'args lenv) caseAlt'rhs
              | otherwise          = match cs
            match [] = ValBottom
            --
            bindParams []             []             = id
            bindParams (v:vs) (Typed n _:ts) = bindParams vs ts . Map.insert n v
            bindParams _ _ = error "Type error in case"
        _ -> ValBottom
      EConstr _ tag arity    -> constr tag arity
      --
      EBottom{} -> ValBottom

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

primVals :: Map.Map Name Val
primVals = fmap evalD builtInDiadic <> fmap evalD builtInUnary
  where
    evalD = \case
      Add -> lift2 ((+) @Int64)
      Mul -> lift2 ((*) @Int64)
      Sub -> lift2 ((-) @Int64)
      Div -> lift2 (div @Int64)
      Neg -> lift1 (negate @Int64)
      -- Polymorphic ops
      Eq -> opComparison (==)
      Ne -> opComparison (/=)
      Lt -> opComparison (<)
      Le -> opComparison (<=)
      Gt -> opComparison (>)
      Ge -> opComparison (>=)
      --
      And -> lift2 (&&)
      Or  -> lift2 (||)
      Xor -> lift2 $ \a b -> (a || b) && not (a && b)
      Not -> lift1 not
      --
      SigAnd  -> lift2 $ \a b -> Fix $ SigmaAnd [a,b]
      SigOr   -> lift2 $ \a b -> Fix $ SigmaOr  [a,b]
      SigPk   -> lift1 $ \t   -> Fix . SigmaPk <$> publicKeyFromText t
      SigBool -> lift1 $ Fix . SigmaBool
      --
      TextLength  -> lift1 (fromIntegral @_ @Int64 . T.length)
      TextAppend  -> lift2 ((<>) @Text)
      BytesAppend -> lift2 ((<>) @ByteString)
      ToBytes{}   -> undefined
      FromBytes{} -> undefined
      HashBlake   -> error "Blake2b is not implemented yet"
      HashSha     -> lift1 $ \bs -> let Hash h = hashBlob @SHA256 bs in h
      Sha256      -> lift1 $ \bs -> let Hash h = hashBlob @SHA256 bs in h
      ShowInt     -> lift1 (T.pack . show @Int64)
      ShowBool    -> lift1 (T.pack . show @Bool)
      Bottom      -> ValBottom
      -- G-machine primitives      
      Unwind       -> gerr
      PushGlobal{} -> gerr
      PushPrim{}   -> gerr
      PushBasic{}  -> gerr
      Push{}       -> gerr
      Mkap         -> gerr
      Update{}     -> gerr
      Pop{}        -> gerr
      Slide{}      -> gerr
      Alloc{}      -> gerr
      Eval         -> gerr
      Cond{}       -> gerr
      Pack{}       -> gerr
      CaseJump{}   -> gerr
      Split{}      -> gerr
      Print        -> gerr
      MkPrim       -> gerr
      Get          -> gerr
      UpdatePrim{} -> gerr
    gerr = error "GMachine primitive"

opComparison :: (forall a. Ord a => a -> a -> Bool) -> Val
opComparison (#) = primFun2 go
  where
    go (PrimInt   a) (PrimInt   b) = ValP $ PrimBool $ a # b
    go (PrimBool  a) (PrimBool  b) = ValP $ PrimBool $ a # b
    go (PrimText  a) (PrimText  b) = ValP $ PrimBool $ a # b
    go (PrimBytes a) (PrimBytes b) = ValP $ PrimBool $ a # b
    -- FIXME: Comparison for sigma expressions?
    go (PrimSigma _) (PrimSigma _) = ValBottom
    go _ _ = ValBottom
    
primFun2 :: (Prim -> Prim -> Val) -> Val
primFun2 f = Val2F go
  where
    go (ValP a) (ValP b) = f a b
    go _        _        = ValBottom


----------------------------------------------------------------
-- Lifting of functions
----------------------------------------------------------------

class MatchPrim a where
  matchP :: Prim -> Maybe a

class InjPrim a where
  inj :: a -> Val

instance MatchPrim Int64      where  matchP p = [ a | PrimInt   a <- pure p ]
instance MatchPrim Bool       where  matchP p = [ a | PrimBool  a <- pure p ]
instance MatchPrim Text       where  matchP p = [ a | PrimText  a <- pure p ]
instance MatchPrim ByteString where  matchP p = [ a | PrimBytes a <- pure p ]

instance k ~ PublicKey => MatchPrim (Sigma k) where
  matchP p = [ a | PrimSigma a <- pure p ]

instance InjPrim Int64      where inj = ValP . PrimInt
instance InjPrim Bool       where inj = ValP . PrimBool
instance InjPrim Text       where inj = ValP . PrimText
instance InjPrim ByteString where inj = ValP . PrimBytes

instance k ~ PublicKey => InjPrim (Sigma k) where
  inj = ValP . PrimSigma

instance InjPrim a => InjPrim (Maybe a) where
  inj = maybe ValBottom inj

lift1 :: (MatchPrim a, InjPrim b) => (a -> b) -> Val
lift1 f = ValF go
  where
    go (ValP a) = inj $ f <$> matchP a
    go _        = ValBottom

lift2 :: (MatchPrim a, MatchPrim b, InjPrim c) => (a -> b -> c) -> Val
lift2 f = Val2F go
  where
    go (ValP a) (ValP b) = inj $ f <$> matchP a <*> matchP b
    go _        _        = ValBottom

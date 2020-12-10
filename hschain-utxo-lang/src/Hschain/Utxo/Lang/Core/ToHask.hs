{-# LANGUAGE ViewPatterns #-}
-- | Converts ExprCore to haskell expression.
-- We ues it to borrow pretty-printer from haskell-src-exts
module Hschain.Utxo.Lang.Core.ToHask(
    toHaskExprCore
  , IsVarName
  , fromTypeCore
) where

import Hex.Common.Text (showt)

import Control.Monad.State.Strict
import Data.ByteString (ByteString)
import Data.Fix
import Data.Char
import Data.List (unfoldr)
import Data.Void
import Data.Word
import Data.Text (Text)

import HSChain.Crypto.Classes (ByteRepr(..), encodeBase58)

import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Expr (monoPrimopName)

import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Pretty as H
import qualified Hschain.Utxo.Lang.Const as Const
import qualified Data.Text as T


class IsVarName a where
  toVarName :: a -> Text

instance IsVarName Text where
  toVarName = id

instance IsVarName Void where
  toVarName = absurd

toHaskExprCore :: IsVarName a => Core a -> H.Exp ()
toHaskExprCore = flip evalState (T.pack . ("x_"++) <$> identifierStream) . go
  where
    go = \case
      EVar v     -> pure $ toVar $ toVarName v
      BVar i     -> do x <- freshName
                       pure $ toVar $ x <> "@" <> showt i
      EPrim p    -> pure $ fromPrim p
      EPrimOp op -> pure $ toVar $ toOpName op
      ELam ty body -> do
        (pats, expr) <- getLamPats ty body
        H.Lambda () (fmap (uncurry toPat) pats) <$> go expr
      EAp f a            -> H.App () <$> go f <*> go a
      ELet rhs body -> do
        (binds, expr) <- getLetBinds rhs body
        H.Let () <$> toBinds binds <*> go expr
      EIf c t e          -> H.If () <$> go c <*> go t <*> go e
      ECase e alts       -> H.Case () <$> go e <*> traverse fromAlt alts
      EConstr con        -> let hcon = H.Con () (toQName $ conName con)
                            in pure $  maybe hcon ((\t -> H.ExpTypeSig () hcon t) . fromTypeCore) (conCoreType con)
      EBottom            -> pure $ H.Var () (toQName "bottom")

    freshName = get >>= \case
      []   -> error "toHaskExprCore: out of names"
      n:ns -> n <$ put ns

    toVar = H.Var () . toQName
    toPat name ty = H.PatTypeSig () (H.PVar () (toName name)) (fromTypeCore ty)

    getLamPats ty expr = do
      x <- freshName
      case expr of
        ELam ty' body -> do (pats,e) <- getLamPats ty' body
                            pure ((x,ty):pats, e)
        _             -> pure ([(x,ty)], expr)

    getLetBinds rhs expr = do
      x <- freshName
      case expr of
        ELet rhs' body -> do (binds,e) <- getLetBinds rhs' body
                             pure ((x,rhs):binds, e)
        _              -> pure ([(x,rhs)], expr)

    fromPrim = \case
      PrimInt n     -> H.Lit () $ H.Int () (fromIntegral n) (show n)
      PrimText txt  -> fromText txt
      PrimBytes bs  -> H.App () (H.Var () (toQName "bytes")) (fromText $ encodeBase58 bs)
      PrimBool b    -> fromBool b
      PrimSigma sig -> fromSigma $ mapPk encodeToBS sig

    fromText txt = H.Lit () $ H.String () str str
      where
        str = T.unpack txt

    fromBool b = H.Con () $ if b then toQName "True" else toQName "False"

    fromSigma :: Sigma ByteString -> H.Exp ()
    fromSigma = cata rec
      where
        rec = \case
          SigmaPk pkey -> let keyTxt = encodeBase58 pkey
                          in  ap1 Const.pk $ lit $ H.String src (T.unpack keyTxt) (T.unpack keyTxt)
          SigmaAnd as  -> foldl1 (ap2 Const.sigmaAnd) as
          SigmaOr  as  -> foldl1 (ap2 Const.sigmaOr) as
          SigmaBool b  -> fromBool b

        ap1 f a = H.App () (toVar f) a
        ap2 f a b = H.App src (H.App () (toVar f) a) b

        lit = H.Lit ()
        src = ()

    toOpName :: PrimOp TypeCore -> Text
    toOpName = \case
      op | Just name <- monoPrimopName op -> name
      OpSigListAll ty                     -> Const.allSigma =: ty
      OpSigListAny ty                     -> Const.anySigma =: ty
      OpShow ty                           -> Const.show =: ty
      OpListMap a b                       -> Const.map =: a =: b
      OpListAt a                          -> Const.listAt =: a
      OpListAppend a                      -> Const.appendList =: a
      OpListLength a                      -> Const.length =: a
      OpListFoldr a b                     -> Const.foldr =: a =: b
      OpListFoldl a b                     -> Const.foldl =: a =: b
      OpListFilter a                      -> Const.filter =: a
      OpListAll a                         -> Const.all =: a
      OpListAny a                         -> Const.any =: a
      OpGT a                              -> Const.greater =: a
      OpLT a                              -> Const.less =: a
      OpGE a                              -> Const.greaterEquals =: a
      OpLE a                              -> Const.lessEquals =: a
      OpEQ a                              -> Const.equals =: a
      OpNE a                              -> Const.nonEquals =: a
      other                               -> showt other
      where
        (=:) name t = mconcat [name, "@", T.pack $ H.prettyPrint $ fromTypeCore t]

    fromAlt CaseAlt{..} = do
      pats <- fmap (H.PVar () . toName) <$> replicateM caseAlt'nVars freshName
      rhs  <- H.UnGuardedRhs () <$> go caseAlt'rhs
      pure $ H.Alt () (H.PApp () cons pats) rhs Nothing
      where
        cons = toQName $ "Con" <> showt caseAlt'tag

    toBinds xs = H.BDecls () <$> traverse toBind xs
      where
        toBind (name, expr) = do
          hs <- go expr
          pure $ H.FunBind () [H.Match () (toName name) [] (H.UnGuardedRhs () hs) Nothing]


fromTypeCore :: TypeCore -> H.Type ()
fromTypeCore = \case
  IntT     -> tyCon "Int"
  BoolT    -> tyCon "Bool"
  BytesT   -> tyCon "Bytes"
  TextT    -> tyCon "Text"
  SigmaT   -> tyCon "Sigma"
  a :-> b  -> H.TyFun () (fromTypeCore a) (fromTypeCore b)
  ListT t  -> H.TyList () (fromTypeCore t)
  TupleT ts -> H.TyTuple () H.Boxed (fmap fromTypeCore ts)
  BoxT      -> tyCon "Box"
  UnitT     -> H.TyTuple () H.Boxed []
  MaybeT a  -> H.TyApp () (tyCon "Maybe") (fromTypeCore a)
  SumT ts   -> foldl (H.TyApp ()) (tyCon $ "Sum" <> showt (length ts)) $ fmap fromTypeCore ts
  where
    tyCon = H.TyCon () . toQName

toQName :: Text -> H.QName ()
toQName name = H.UnQual () $ toName name

toName :: Text -> H.Name ()
toName name = H.Ident () $ T.unpack name


-- LGC name generator
--
-- We need way to conjure names for variables and sequential numbering
-- is too repetitive. So we use LCG in order to generate sequence of
-- non-repeating N-grams for use as identifiers. Here's definition of
-- LCG:
--
-- > x[n+1] = a·x[n] + c  `mod` m
--
-- Since we want to generate N-grams we should choose `m` as 26^n and
-- for a suitable choice of `a` & `c` we'll get full period generator
-- which will produce 26^n N-grams shuffled in some way. They will
-- appear random for reader which is quite sufficient for our purposes
--
-- Here is requirements on choice of coefficients (Hull–Dobell Theorem):
--
--  1. `m` and `c` are relatively prime,
--  2. `a − 1` is divisible by all prime factors of `m`
--  3. `a − 1` is divisible by 4 if `m` is divisible by 4.
--
-- They could be satisfied by following choices:
--
--   m = 26^n
--   a = 26^(n-1) + 1
--   c = 17^n
--
-- This is not the best generator but sequence looks random enough and
-- what important there's no duplicates!

-- Effectively infinite stream of unique names
identifierStream :: [String]
identifierStream = concat [ randomNames (lcgName n) | n <- [4..10]]

data LCGName = LCGName
  { lcgM :: !Word64
  , lcgA :: !Word64
  , lcgC :: !Word64
  , lcgN :: !Int
  }

lcgName :: Int -> LCGName
lcgName n
  | n < 3     = error "lcgName: we won't get full period"
  | n > 13    = error "lcgName: Word64 will overflow"
  | otherwise = LCGName { lcgM = 26^n
                        , lcgA = 26^(n-1) + 1
                        , lcgC = 17^n
                        , lcgN = n
                        }

randomNames :: LCGName -> [String]
randomNames lcg@LCGName{..}
  = map (toIdent lcg) $ take (26^lcgN) $ iterate step 0
  where
    step x = (lcgA*x + lcgC) `mod` lcgM

toIdent :: LCGName -> Word64 -> String
toIdent LCGName{lcgN=n} = take n . unfoldr toC
  where
    toC ((`divMod` 26) -> (x',c)) = Just (chr $ ord 'a' + fromIntegral c, x')

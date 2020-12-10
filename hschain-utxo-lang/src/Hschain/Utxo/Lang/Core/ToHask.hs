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
import Data.String
import Data.Void
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
toHaskExprCore = flip evalState (T.pack <$> stringPrettyLetters) . go []
  where
    go env = \case
      EVar v     -> pure $ toVar $ toVarName v
      BVar i     -> pure $ toVar $ env !! i <> "@" <> showt i
      EPrim p    -> pure $ fromPrim p
      EPrimOp op -> pure $ toVar $ toOpName op
      ELam ty body -> do
        (pats, expr) <- getLamPats env ty body
        pure $ H.Lambda () pats expr
      EAp f a       -> H.App () <$> go env f <*> go env a
      ELet rhs body -> do
        (binds, expr) <- getLetBinds env rhs body
        pure $ H.Let () (H.BDecls () binds) expr
      EIf c t e          -> H.If () <$> go env c <*> go env t <*> go env e
      ECase e alts       -> H.Case () <$> go env e <*> traverse (fromAlt env) alts
      EConstr con        -> let hcon = H.Con () (toQName $ conName con)
                            in pure $  maybe hcon ((\t -> H.ExpTypeSig () hcon t) . fromTypeCore) (conCoreType con)
      EBottom            -> pure $ H.Var () (toQName "bottom")

    freshName = get >>= \case
      []   -> error "toHaskExprCore: out of names"
      n:ns -> n <$ put ns

    toVar = H.Var () . toQName
    toPat name ty = H.PatTypeSig () (H.PVar () (toName name)) (fromTypeCore ty)

    getLamPats env ty expr = do
      x <- freshName
      case expr of
        ELam ty' body -> do (pats,e) <- getLamPats (x:env) ty' body
                            pure (toPat x ty:pats, e)
        _             -> do e <- go (x:env) expr
                            pure ([toPat x ty], e)

    getLetBinds env rhs expr = do
      x    <- freshName
      bind <- toBind env x rhs
      case expr of
        ELet rhs' body -> do (binds,e) <- getLetBinds (x:env) rhs' body
                             pure (bind:binds, e)
        _              -> do e <- go (x:env) expr
                             pure ([bind], e)

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

    fromAlt env CaseAlt{..} = do
      names <- replicateM caseAlt'nVars freshName
      let pats = H.PVar () . toName <$> names
      rhs  <- H.UnGuardedRhs () <$> go (names <> env) caseAlt'rhs
      pure $ H.Alt () (H.PApp () cons pats) rhs Nothing
      where
        cons = toQName $ "Con" <> showt caseAlt'tag

    -- toBinds xs = H.BDecls () <$> traverse toBind xs
    --   where
    toBind env name expr = do
      hs <- go env expr
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

stringPrettyLetters :: IsString a => [a]
stringPrettyLetters = fmap fromString $ [1..] >>= flip replicateM ['a'..'z']

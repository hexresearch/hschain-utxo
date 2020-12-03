-- | Converts ExprCore to haskell expression.
-- We ues it to borrow pretty-printer from haskell-src-exts
module Hschain.Utxo.Lang.Core.ToHask(
    toHaskExprCore
  , IsVarName
  , fromTypeCore
) where

import Hex.Common.Text (showt)

import Data.ByteString (ByteString)
import Data.Fix
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
toHaskExprCore = \case
  EVar v             -> toVar $ toVarName v
  BVar i             -> toVar $ T.pack $ "@" ++ show i
  EPrim p            -> fromPrim p
  EPrimOp op         -> toVar $ toOpName op
  ELam ty body ->
    let (ps, expr) = getLamPats [("_", ty)] body
    in  H.Lambda () (fmap (uncurry toPat) ps) $ rec expr
  EAp f a            -> H.App () (rec f) (rec a)
  ELet rhs body ->
    let (bs, expr) = getLetBinds [("_", rhs)] body
    in  H.Let () (toBinds bs) (rec expr)
  EIf c t e          -> H.If () (rec c) (rec t) (rec e)
  ECase e alts       -> H.Case () (rec e) (fmap fromAlt alts)
  EConstr con        -> let hcon = H.Con () (toQName $ conName con)
                        in  maybe hcon ((\t -> H.ExpTypeSig () hcon t) . fromTypeCore) (conCoreType con)
  EBottom            -> H.Var () (toQName "bottom")
  where
    rec = toHaskExprCore

    toVar = H.Var () . toQName
    toPat name ty = H.PatTypeSig () (H.PVar () (toName name)) (fromTypeCore ty)

    getLamPats res = \case
      ELam ty body -> getLamPats (("_", ty) : res) body
      other        -> (reverse res, other)

    getLetBinds res = \case
      ELet rhs body -> getLetBinds (("_", rhs) : res) body
      other         -> (reverse res, other)

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
    fromSigma x = cata go x
      where
        go = \case
          SigmaPk pkey -> let keyTxt = encodeBase58 pkey
                          in  ap Const.pk $ lit $ H.String src (T.unpack keyTxt) (T.unpack keyTxt)
          SigmaAnd as  -> foldl1 (ap2 Const.sigmaAnd) as
          SigmaOr  as  -> foldl1 (ap2 Const.sigmaOr) as
          SigmaBool b  -> fromBool b

        ap f a = H.App () (toVar f) a
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

    fromAlt CaseAlt{..} = H.Alt () (H.PApp () cons pats) (toRhs caseAlt'rhs) Nothing
      where
        cons = toQName $ "Con" <> showt caseAlt'tag
        pats = fmap (H.PVar () . toName) $ replicate caseAlt'nVars "_"

    toBinds xs = H.BDecls () $ fmap toBind xs
      where
        toBind (name, expr) = H.FunBind ()
          [H.Match () (toName name) [] (H.UnGuardedRhs () $ rec expr) Nothing]

    toRhs expr = H.UnGuardedRhs () $ rec expr

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

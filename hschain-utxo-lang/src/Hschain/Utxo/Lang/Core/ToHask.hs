-- | Converts ExprCore to haskell expression.
-- We ues it to borrow pretty-printer from haskell-src-exts
module Hschain.Utxo.Lang.Core.ToHask(
  -- toHaskExprCore
) where

{-
import Hex.Common.Text (showt)

import Data.ByteString (ByteString)
import Data.Fix
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

toHaskExprCore :: ExprCore -> H.Exp ()
toHaskExprCore = \case
  EVar v             -> toVar v
  EPrim p            -> fromPrim p
  EPrimOp op         -> toVar $ toOpName op
  ELam name ty body  -> let (ps, expr) = getLamPats [(name, ty)] body
                        in  H.Lambda () (fmap (uncurry toPat) ps) $ rec expr
  EAp f a            -> H.App () (rec f) (rec a)
  ELet name rhs body -> let (bs, expr) = getLetBinds [(name, rhs)] body
                        in  H.Let () (toBinds bs) (rec expr)
  EIf c t e          -> H.If () (rec c) (rec t) (rec e)
  ECase e alts       -> H.Case () (rec e) (fmap fromAlt alts)
  EConstr ty n       -> H.ExpTypeSig () (H.Con () (toQName $ "Con" <> showt n )) (fromType ty)
  EBottom            -> H.Var () (toQName "bottom")
  where
    rec = toHaskExprCore

    toQName name = H.UnQual () $ toName name
    toName name = H.Ident () $ T.unpack name
    toVar = H.Var () . toQName
    toPat name ty = H.PatTypeSig () (H.PVar () (toName name)) (fromType ty)

    getLamPats res = \case
      ELam name ty body -> getLamPats ((name, ty) : res) body
      other             -> (reverse res, other)

    getLetBinds res = \case
      ELet name rhs body -> getLetBinds ((name, rhs) : res) body
      other              -> (reverse res, other)

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
        (=:) name t = mconcat [name, "@", T.pack $ H.prettyPrint $ fromType t]

    fromType :: TypeCore -> H.Type ()
    fromType = \case
      IntT     -> tyCon "Int"
      BoolT    -> tyCon "Bool"
      BytesT   -> tyCon "Bytes"
      TextT    -> tyCon "Text"
      SigmaT   -> tyCon "Sigma"
      a :-> b  -> H.TyFun () (fromType a) (fromType b)
      ListT t  -> H.TyList () (fromType t)
      TupleT ts -> H.TyTuple () H.Boxed (fmap fromType ts)
      BoxT      -> tyCon "Box"
      where
        tyCon = H.TyCon () . toQName

    fromAlt :: CaseAlt -> H.Alt ()
    fromAlt CaseAlt{..} = H.Alt () (H.PApp () cons pats) (toRhs caseAlt'rhs) Nothing
      where
        cons = toQName $ "Con" <> showt caseAlt'tag
        pats = fmap (H.PVar () . toName) caseAlt'args

    toBinds :: [(Name, ExprCore)] -> H.Binds ()
    toBinds xs = H.BDecls () $ fmap toBind xs
      where
        toBind (name, expr) = H.FunBind () [H.Match () (toName name) [] (toRhs expr) Nothing]

    toRhs expr = H.UnGuardedRhs () $ rec expr

-}

module Hschain.Utxo.Lang.Compile.Hask.Utils(
    toName
  , toQName
  , toType
  , toPat
  , toTypedPat
  , toSigma
  , toText
  , toBool
) where

import Hex.Common.Text

import Data.Text (Text)

import Hschain.Utxo.Lang.Expr (Loc, VarName(..))
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Type, SigmaExpr(..), Typed(..))
import Hschain.Utxo.Lang.Sigma (publicKeyToText)

import qualified Data.List as L
import qualified Data.Text as T

import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.HM as H(LocFunctor(..))

import qualified Hschain.Utxo.Lang.Parser.Hask.ToHask as H(toHaskType)

toName :: VarName -> H.Name Loc
toName (VarName loc txt) = H.Ident loc $ T.unpack txt

toQName :: VarName -> H.QName Loc
toQName (VarName loc txt) = H.UnQual loc $ H.Ident loc $ T.unpack txt

toType :: Loc -> Type -> H.Type Loc
toType loc ty = H.toHaskType $ H.mapLoc (const loc) ty

toPat :: Loc -> Name -> H.Pat Loc
toPat loc name = H.PVar loc $ toName (VarName loc name)

toTypedPat :: Loc -> Typed Name -> H.Pat Loc
toTypedPat loc (Typed name ty) = H.PatTypeSig loc (H.PVar loc $ toName $ VarName loc name) (toType loc ty)

toSigma :: Loc -> SigmaExpr -> H.Exp Loc
toSigma loc = \case
  SigmaBool b  -> H.App loc (H.Var loc $ toQName $ VarName loc "toSigma") (toBool loc b)
  SigmaAnd  as -> sigmaOp "&&" as
  SigmaOr   as -> sigmaOp "||" as
  SigmaPk   pk -> H.App loc (H.Var loc $ toQName $ VarName loc "pk") (toText loc $ publicKeyToText pk)
  where
    rec = toSigma loc
    sigmaOp op args = L.foldr1 (\a b -> H.InfixApp loc a (toQOp op) b) $ fmap rec args
    toQOp op = H.QVarOp loc (toQName $ VarName loc op)

toText :: Loc -> Text -> H.Exp Loc
toText loc txt = H.Lit loc $ H.String loc (T.unpack txt) (T.unpack txt)

toBool :: Loc -> Bool -> H.Exp Loc
toBool loc b = H.Con loc $ toQName $ VarName loc $ showt b

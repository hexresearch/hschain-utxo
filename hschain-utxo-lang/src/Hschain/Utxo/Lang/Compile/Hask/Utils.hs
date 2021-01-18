module Hschain.Utxo.Lang.Compile.Hask.Utils(
    toName
  , toQName
  , toType
  , toPat
  , toTypedPat
  , toSigma
  , toText
  , toBool
  , toBytes
) where

import Hex.Common.Text (showt)

import Data.Fix
import Data.ByteString (ByteString)
import Data.Text (Text)

import HSChain.Crypto.Classes (encodeBase58)
import Hschain.Utxo.Lang.Expr (Loc, VarName(..))
import Hschain.Utxo.Lang.Core.Types (Name, Typed(..))
import Hschain.Utxo.Lang.Sigma

import qualified Data.List as L
import qualified Data.Text as T

import qualified Language.Haskell.Exts.Syntax as H
import qualified Type.Check.HM as HM(LocFunctor(..), Type)

import qualified Hschain.Utxo.Lang.Parser.Hask.ToHask as H(toHaskType)
import qualified Hschain.Utxo.Lang.Sigma.Protocol as Sigma
import qualified Hschain.Utxo.Lang.Sigma.DTuple as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types as Sig

toName :: VarName -> H.Name Loc
toName (VarName loc txt) = H.Ident loc $ T.unpack txt

toQName :: VarName -> H.QName Loc
toQName (VarName loc txt) = H.UnQual loc $ H.Ident loc $ T.unpack txt

toType :: Loc -> HM.Type () Name -> H.Type Loc
toType loc ty = H.toHaskType $ HM.mapLoc (const loc) ty

toPat :: Loc -> Name -> H.Pat Loc
toPat loc name = H.PVar loc $ toName (VarName loc name)

toTypedPat :: Loc -> Typed (HM.Type () Name) Name -> H.Pat Loc
toTypedPat loc (Typed name ty) = H.PatTypeSig loc (H.PVar loc $ toName $ VarName loc name) (toType loc ty)

toSigma :: Loc -> Sigma ProofInput -> H.Exp Loc
toSigma loc = foldFix $ \case
  SigmaBool b  -> H.App loc (H.Var loc $ toQName $ VarName loc "toSigma") (toBool loc b)
  SigmaAnd  as -> sigmaOp "&&" as
  SigmaOr   as -> sigmaOp "||" as
  SigmaPk   pk -> fromProofInput pk
  where
    sigmaOp op args = L.foldr1 (\a b -> H.InfixApp loc a (toQOp op) b) args
    toQOp op = H.QVarOp loc (toQName $ VarName loc op)

    fromProofInput = \case
      Sigma.InputDLog   pk -> app loc "pk" [toText loc $ publicKeyToText pk]
      Sigma.InputDTuple dt -> app loc "proofDTuple" $ fmap (toText loc . publicKeyToText . Sig.PublicKey)
        [ Sigma.dtuple'g    dt
        , Sigma.dtuple'g_x  dt
        , Sigma.dtuple'g_y  dt
        , Sigma.dtuple'g_xy dt
        ]


toBytes :: Loc -> ByteString -> H.Exp Loc
toBytes loc bs = app loc "pack58" [toText loc (encodeBase58 bs)]

toText :: Loc -> Text -> H.Exp Loc
toText loc txt = H.Lit loc $ H.String loc (T.unpack txt) (T.unpack txt)

toBool :: Loc -> Bool -> H.Exp Loc
toBool loc b = H.Con loc $ toQName $ VarName loc $ showt b

app :: Loc -> Text -> [H.Exp Loc] -> H.Exp Loc
app loc name args = L.foldl' (H.App loc) op args
  where
    op = H.Var loc $ toQName $ VarName loc name

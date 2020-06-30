-- | Functions to convert extended lambda-calculus to haskell-like programsm.
-- We use it to borrow pretty-printer from the package haskell-src-exts.
module Hschain.Utxo.Lang.Compile.Hask.ToHask(
    toHaskProg
  , toHaskDecl
  , toHaskExpr
) where

import Hex.Common.Text

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Expr (Loc, noLoc)
import Hschain.Utxo.Lang.Sigma (publicKeyToText)
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Type, Prim(..), SigmaExpr(..), Typed(..))

import Data.Fix
import Data.Text (Text)

import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.HM as H(getLoc, LocFunctor(..))
import qualified Data.List as L
import qualified Data.Text as T

import qualified Hschain.Utxo.Lang.Parser.Hask.ToHask as H(toHaskType)

toHaskProg :: CoreProg -> H.Module Loc
toHaskProg (CoreProg prog) = H.Module noLoc modHead modPragmas modImports decls
  where
    modHead    = Nothing
    modPragmas = []
    modImports = []

    decls = fmap toHaskDecl prog

toHaskDecl :: Comb Name -> H.Decl Loc
toHaskDecl Def{..} = H.FunBind defLoc [match]
  where
    match  = H.Match defLoc name pats rhs binds

    rhs    = H.UnGuardedRhs defLoc $ toHaskExpr def'body
    pats   = fmap (toPat defLoc) def'args

    defLoc = H.getLoc def'body
    name   = toName defLoc def'name
    binds  = Nothing

toPat :: Loc -> Name -> H.Pat Loc
toPat loc name = H.PVar loc $ toName loc name

toTypedPat :: Loc -> Typed Name -> H.Pat Loc
toTypedPat loc (Typed name ty) = H.PatTypeSig loc (H.PVar loc $ toName loc name) (toType loc ty)

toHaskExpr :: Expr Name -> H.Exp Loc
toHaskExpr = cata $ \case
  EVar loc name            -> toVar loc name
  EPrim loc p              -> toPrim loc p
  EAp loc f a              -> toAp loc f a
  ELet loc bs a            -> toLet loc bs a
  ELam loc args a          -> toLam loc args a
  EIf loc c t e            -> toIf loc c t e
  ECase loc e alts         -> toCase loc e alts
  EConstr loc ty tag arity -> toConstr loc ty tag arity
  EBottom loc              -> toBottom loc
  where
    toVar loc name = H.Var loc $ toQName loc name

    toPrim loc = \case
      PrimInt   n      -> H.Lit loc $ H.Int loc (fromIntegral n) (show n)
      PrimText  txt    -> toText loc txt
      PrimBool  b      -> toBool loc b
      PrimSigma sigma  -> toSigma loc sigma

    toAp = H.App

    toLet loc bs e = H.Let loc (H.BDecls loc $ fmap (toBind loc) bs) e

    toLam loc args e = H.Lambda loc (fmap (toPat loc) args) e

    toIf = H.If

    toCase loc e alts = H.Case loc e $ fmap toAlt alts

    toConstr loc ty tag arity = H.ExpTypeSig loc (H.Con loc (toQName loc $ constrName tag arity)) (toType loc ty)

    toBottom loc = toVar loc "undefined"

    constrName tag arity = mconcat ["Con_", showt tag, "_", showt arity]

    toBind loc (name, e) = H.FunBind loc [match]
      where
        match = H.Match loc (toName loc name) [] (H.UnGuardedRhs loc e) Nothing

    toAlt CaseAlt{..} = H.Alt loc pat rhs Nothing
      where
        pat = H.PatTypeSig loc (H.PApp loc (toQName loc $ constrName caseAlt'tag (length caseAlt'args)) (fmap (toTypedPat loc)  caseAlt'args)) (toType loc caseAlt'constrType)
        loc = caseAlt'loc
        rhs = H.UnGuardedRhs loc caseAlt'rhs

toName :: Loc -> Name -> H.Name Loc
toName loc txt = H.Ident loc $ T.unpack txt

toQName :: Loc -> Name -> H.QName Loc
toQName loc txt = H.UnQual loc $ H.Ident loc $ T.unpack txt

toType :: Loc -> Type -> H.Type Loc
toType loc ty = H.toHaskType $ H.mapLoc (const loc) ty

toSigma :: Loc -> SigmaExpr -> H.Exp Loc
toSigma loc = \case
  SigmaBool b  -> H.App loc (H.Var loc $ toQName loc "toSigma") (toBool loc b)
  SigmaAnd  as -> sigmaOp "&&" as
  SigmaOr   as -> sigmaOp "||" as
  SigmaPk   pk -> H.App loc (H.Var loc $ toQName loc "pk") (toText loc $ publicKeyToText pk)
  where
    rec = toSigma loc
    sigmaOp op args = L.foldr1 (\a b -> H.InfixApp loc a (toQOp op) b) $ fmap rec args
    toQOp op = H.QVarOp loc (toQName loc op)

toText :: Loc -> Text -> H.Exp Loc
toText loc txt = H.Lit loc $ H.String loc (T.unpack txt) (T.unpack txt)

toBool :: Loc -> Bool -> H.Exp Loc
toBool loc b = H.Con loc $ toQName loc $ showt b


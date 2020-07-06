-- | Functions to convert typed extended lambda-calculus to haskell-like programms.
-- We use it to borrow pretty-printer from the package haskell-src-exts.
module Hschain.Utxo.Lang.Compile.Hask.TypedToHask(
    toHaskProg
  , toHaskDecl
  , toHaskExpr
) where

import Hex.Common.Text

import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Hask.Utils
import Hschain.Utxo.Lang.Expr (Loc, noLoc, VarName(..))
import Hschain.Utxo.Lang.Core.Data.Prim (Prim(..), Typed(..))

import Data.Fix

import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.HM as H(getLoc, LocFunctor(..))

import qualified Hschain.Utxo.Lang.Parser.Hask.ToHask as H(toHaskType)

toHaskProg :: TypedProg -> H.Module Loc
toHaskProg (AnnProg prog) = H.Module noLoc modHead modPragmas modImports decls
  where
    modHead    = Nothing
    modPragmas = []
    modImports = []

    decls = toHaskDecl =<< prog

toHaskDecl :: TypedDef -> [H.Decl Loc]
toHaskDecl defn = [typeSigDecl defn, funDecl defn]

typeSigDecl :: TypedDef -> H.Decl Loc
typeSigDecl def@Def{..} =
  H.TypeSig loc [toName def'name] (toType loc $ getTypedDefType def)
  where
    loc = varName'loc def'name

funDecl :: TypedDef -> H.Decl Loc
funDecl Def{..} = H.FunBind defLoc [match]
  where
    match  = H.Match defLoc name pats rhs binds

    rhs    = H.UnGuardedRhs defLoc $ toHaskExpr def'body
    pats   = fmap (toTypedPat defLoc) def'args

    defLoc = varName'loc def'name
    name   = toName def'name
    binds  = Nothing

toHaskExpr :: TypedExpr -> H.Exp Loc
toHaskExpr = cata $ \case
  Ann exprTy expr -> withSig (H.getLoc expr) exprTy $ case expr of
    EVar loc name              -> toVar loc name
    EPrim _ (PrimLoc ploc p)   -> toPrim ploc p
    EAp loc f a                -> toAp loc f a
    ELet loc bs a              -> toLet loc bs a
    ELam loc args a            -> toLam loc args a
    EIf loc c t e              -> toIf loc c t e
    ECase loc e alts           -> toCase loc e alts
    EConstr loc ty tag arity   -> toConstr loc ty tag arity
    EBottom loc                -> toBottom loc
  where
    withSig loc ty e = H.ExpTypeSig loc e (H.toHaskType $ H.mapLoc (const noLoc) ty)

    toVar loc name = H.Var loc $ toQName $ VarName loc name

    toPrim loc = \case
      PrimInt   n      -> H.Lit loc $ H.Int loc (fromIntegral n) (show n)
      PrimText  txt    -> toText loc txt
      PrimBool  b      -> toBool loc b
      PrimSigma sigma  -> toSigma loc sigma

    toAp = H.App

    toLet loc bs e = H.Let loc (H.BDecls loc $ toBind loc =<< bs) e

    toLam loc args e = H.Lambda loc (fmap (toTypedPat loc) args) e

    toIf = H.If

    toCase loc e alts = H.Case loc e $ fmap toAlt alts

    toConstr loc ty tag arity = H.ExpTypeSig loc (H.Con loc (toQName $ VarName loc $ constrName tag arity)) (toType loc ty)

    toBottom loc = toVar loc "undefined"

    constrName tag arity = mconcat ["Con_", showt tag, "_", showt arity]

    toTypeSig loc tyName = H.TypeSig loc [toName $ VarName loc name] (toType loc ty)
      where
        name = typed'value tyName
        ty   = typed'type  tyName

    toBind loc (tyName, e) = [toTypeSig loc tyName, H.FunBind loc [match]]
      where
        match = H.Match loc (toName $ VarName loc name) [] (H.UnGuardedRhs loc e) Nothing
        name  = typed'value tyName

    toAlt CaseAlt{..} = H.Alt loc pat rhs Nothing
      where
        pat = H.PatTypeSig loc (H.PApp loc (toQName $ VarName loc $ constrName caseAlt'tag (length caseAlt'args)) (fmap (toTypedPat loc)  caseAlt'args)) (toType loc caseAlt'constrType)
        loc = caseAlt'loc
        rhs = H.UnGuardedRhs loc caseAlt'rhs



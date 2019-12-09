module Hschain.Utxo.Lang.Parser.Hask.FromHask(
    fromHaskExp
  , fromHaskModule
) where

import Control.Applicative
import Control.Monad

import Data.Fix
import Data.String

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Language.Haskell.Exts.Pretty

import Type.Loc
import Type.Type

import Hschain.Utxo.Lang.Expr

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Pretty as H

fromHaskExp :: H.Exp Loc -> ParseResult Lang
fromHaskExp = undefined

fromHaskModule :: H.Module Loc -> ParseResult Module
fromHaskModule = \case
  H.Module loc Nothing [] [] decls -> fromDecls loc decls
  _                                -> err
  where
    err = ParseFailed (H.fromSrcInfo noLoc) "Failed to parse module"

parseFailed loc msg = ParseFailed (H.fromSrcInfo loc) msg

parseFailedBy :: (H.Annotated f, H.Pretty (f Loc)) => String -> f Loc -> ParseResult b
parseFailedBy msg exp = ParseFailed (H.fromSrcInfo $ H.ann exp) $ mconcat [msg, ": ", H.prettyPrint exp]


fromDecls :: Loc -> [H.Decl Loc] -> ParseResult Module
fromDecls loc ds = fmap (Module loc . toBindGroup) $ mapM toDecl ds

data Decl
  = FunDecl Loc [(VarName, Alt Lang)]
  | TypeSig Loc [VarName] Type

toBindGroup :: [Decl] -> [BindGroup Lang]
toBindGroup = undefined

toDecl :: H.Decl Loc -> ParseResult Decl
toDecl = \case
  H.TypeSig loc names ty -> liftA2 (TypeSig loc) (mapM toName names) (toType ty)
  H.FunBind loc matches  -> fmap (FunDecl loc) $ mapM fromMatch matches
  other -> parseFailedBy "Unexpeceted declaration" other
  where
    toName = \case
      H.Ident  loc name -> return $ VarName loc (fromString name)
      other             -> parseFailedBy "Symbol names are not allowed" other

    fromQName = \case
      H.UnQual loc name -> toName name
      other             -> parseFailedBy "Unexpected name" other

    toType = \case
      H.TyFun loc a b -> liftA2 (TFun loc) (rec a) (rec b)
      H.TyTuple loc H.Boxed ts -> fmap (TTuple loc) (mapM rec ts)
      H.TyApp loc a b -> liftA2 (TAp loc) (rec a) (rec b)
      H.TyVar loc name -> fmap (\v -> TVar loc $ Tyvar (getLoc v) (fromVarName v) (Star (getLoc v))) $ toName name
      H.TyCon loc name -> fmap (\v -> TCon loc $ Tycon (getLoc v) (fromVarName v) (Star (getLoc v))) $ fromQName name
      H.TyParen loc ty -> rec ty
      H.TyKind _ ty _ -> rec ty
      other -> parseFailedBy  "Failed to parse type for" other
      where
        rec = toType

    fromMatch = \case
      m@(H.Match loc name pats rhs mBinds) -> liftA2 (,) (toName name) (liftA3 (toAlt loc) (mapM fromPat pats) (fromRhs rhs) (mapM (fromBinds m) mBinds))
      other                            -> parseFailedBy "Failed to parse function bind" other

    toAlt loc pats rhs mBinds = Alt pats (maybe rhs fromBgs mBinds)
      where
        fromBgs bgs = foldr (\a res -> Fix $ Let (getLoc a) a res) rhs bgs

    fromPat = undefined

    fromRhs = \case
      H.UnGuardedRhs _ exp -> fromHaskExp exp
      other                -> parseFailedBy "Failed to parse function" other

    fromBinds topExp = \case
      H.BDecls _ decls -> fmap toBindGroup $ mapM toDecl decls
      _                -> parseFailedBy "Failed to parse binding group for expression" topExp







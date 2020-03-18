module Hschain.Utxo.Lang.Parser.Hask.FromHask(
    fromHaskExp
  , fromHaskModule
  , fromHaskDecl
  , toDecl
) where

import Hex.Common.Text

import Control.Applicative
import Control.Monad

import Data.Fix
import Data.String

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Language.Haskell.Exts.Pretty

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Hask.Dependencies
import Hschain.Utxo.Lang.Parser.Hask.Utils

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Language.HM as HM
import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Parser as H
import qualified Language.Haskell.Exts.Pretty as H

fromHaskExp :: H.Exp Loc -> ParseResult Lang
fromHaskExp topExp = case topExp of
  H.Var loc qname -> fmap (Fix . Var (Just loc)) $ fromQName qname
  H.App loc a b -> liftA2 (\x y -> Fix $ Apply (Just loc) x y) (rec a) (rec b)
  H.Lambda loc ps body -> case ps of
    [p] -> fromLam (Just loc) p body
    _   -> fromLamList (Just loc) ps body
  H.Let loc binds exp -> liftA2 (\x y -> fromBgs y x) (fromBinds topExp binds) (rec exp)
  H.ExpTypeSig loc exp ty -> liftA2 (\x y -> Fix $ Ascr (Just loc) x (HM.typeToSignature y)) (rec exp) (fromType ty)
  H.Lit loc lit -> fmap (Fix . PrimE (Just loc)) $ fromLit lit
  H.If loc a b c -> liftA3 (\x y z -> Fix $ If (Just loc) x y z) (rec a) (rec b) (rec c)
  H.Tuple loc H.Boxed es -> fmap (Fix . Tuple (Just loc) . V.fromList) (mapM rec es)
  H.Con loc qname -> do
    n <- fromQName qname
    let bool b = Fix $ PrimE (Just loc) $ PrimBool b
    case varName'name n of
      "True"  -> return $ bool True
      "False" -> return $ bool False
      _       -> parseFailedBy "Failed to parse expression" topExp
  H.List loc es -> fmap (Fix . VecE (Just loc) . NewVec (Just loc) . V.fromList) (mapM rec es)
  H.InfixApp loc a op b -> fromInfixApp (Just loc) op a b
  H.Paren _ exp         -> rec exp
  H.LeftSection loc a op  -> rec $ unfoldLeftSection loc a op
  H.RightSection loc op a -> rec $ unfoldRightSection loc op a
  other                 -> parseFailedBy "Failed to parse expression" other
  where
    rec = fromHaskExp

    unfoldLeftSection loc a op = H.Lambda loc [H.PVar loc x] (H.InfixApp loc a op (H.Var loc $ H.UnQual loc x))
      where x = H.Ident loc "x"

    unfoldRightSection loc op a = H.Lambda loc [H.PVar loc x] (H.InfixApp loc (H.Var loc $ H.UnQual loc x) op a)
      where x = H.Ident loc "x"

    fromLam loc p body = liftA2  (\x y -> Fix $ Lam loc x y) (fromPatToVar p) (rec body)
    fromLamList loc ps body = liftA2  (\x y -> Fix $ LamList loc x y) (mapM fromPatToVar ps) (rec body)

    fromInfixApp loc op a b =
      liftA3 (\x v y -> Fix $ InfixApply loc x v y) (rec a) (fromOp op) (rec b)

    fromOp = \case
      H.QVarOp loc qname -> fromQName qname
      other              -> parseFailedBy "Failed to parse infix application" other

fromHaskModule :: H.Module Loc -> ParseResult Module
fromHaskModule = \case
  H.Module loc Nothing [] [] decls -> fromDecls (Just loc) decls
  _                                -> err
  where
    err = ParseFailed (H.fromSrcInfo noLoc) "Failed to parse module"

fromHaskDecl :: H.Decl Loc -> ParseResult (BindGroup Lang)
fromHaskDecl d = toBindGroup . return =<< toDecl d

fromDecls :: Maybe Loc -> [H.Decl Loc] -> ParseResult Module
fromDecls loc ds = fmap (Module loc) $ toBindGroup =<< mapM toDecl ds

toDecl :: H.Decl Loc -> ParseResult Decl
toDecl x = case x of
  H.TypeSig loc names ty -> liftA2 (TypeSig loc) (mapM toName names) (fromQualType ty)
  H.FunBind loc matches  -> fmap (FunDecl loc) $ mapM fromMatch matches
  H.PatBind loc pat rhs mBinds -> fromPatBind x loc pat rhs mBinds
  other -> parseFailedBy "Unexpeceted declaration" other
  where
    fromPatBind m loc pat rhs mBinds = liftA2 (\name alt -> FunDecl loc [(name, alt)])
        (getPatName pat)
        (liftA2 (toAlt []) (fromRhs rhs) (mapM (fromBinds m) mBinds))

    fromMatch = \case
      m@(H.Match loc name pats rhs mBinds) -> liftA2 (,) (toName name) (liftA3 toAlt (mapM fromPat pats) (fromRhs rhs) (mapM (fromBinds m) mBinds))
      other                                -> parseFailedBy "Failed to parse function bind" other

    toAlt pats rhs mBinds = Alt pats (maybe rhs (fromBgs rhs) mBinds)
      where

    fromPat = \case
      H.PVar loc name -> fmap (PVar $ Just loc) (toName name)
      other           -> parseFailedBy "Failed to parse patter" other

    fromRhs = \case
      H.UnGuardedRhs _ exp -> fromHaskExp exp
      other                -> parseFailedBy "Failed to parse function" other

    getPatName = \case
      H.PVar loc name -> toName name
      other           -> parseFailedBy "Failed to parse synonym name" other

fromBgs :: Lang -> BindGroup Lang -> Lang
fromBgs rhs bgs = Fix $ Let (HM.getLoc rhs) bgs rhs

fromBinds :: (H.Annotated f, H.Pretty (f Loc)) => f Loc -> H.Binds Loc -> ParseResult (BindGroup Lang)
fromBinds topExp = \case
  H.BDecls _ decls -> toBindGroup =<< mapM toDecl decls
  _                -> parseFailedBy "Failed to parse binding group for expression" topExp

fromPatToVar :: H.Pat Loc -> ParseResult VarName
fromPatToVar = \case
  H.PVar _ name -> toName name
  other         -> parseFailedBy "Failed to parse patter" other


toName :: H.Name Loc -> ParseResult VarName
toName = \case
  H.Ident  loc name -> return $ VarName (Just loc) (fromString name)
  H.Symbol loc name -> return $ VarName (Just loc) (fromString name)

fromQName :: H.QName Loc -> ParseResult VarName
fromQName = \case
  H.UnQual loc name -> toName name
  other             -> parseFailedBy "Unexpected name" other

fromQualType :: H.Type Loc -> ParseResult Signature
fromQualType x = case x of
  H.TyForall loc Nothing mContext ty -> err
  ty -> fmap HM.typeToSignature $ fromType ty
  where
    err = parseFailedBy "Contexts are not allowed" x

fromType :: H.Type Loc -> ParseResult Type
fromType = \case
  H.TyFun loc a b -> liftA2 (HM.arrowT $ Just loc) (rec a) (rec b)
  H.TyTuple loc H.Boxed ts -> fmap (fromTyTuple $ Just loc) (mapM rec ts)
  H.TyApp loc a b -> liftA2 (HM.appT $ Just loc) (rec a) (rec b)
  H.TyVar _ name -> fmap (\VarName{..} -> HM.varT varName'loc varName'name) $ toName name
  H.TyCon _ name -> fmap (\VarName{..} -> HM.conT varName'loc varName'name) $ fromQName name
  H.TyParen loc ty -> rec ty
  H.TyKind _ ty _ -> rec ty
  other -> parseFailedBy  "Failed to parse type for" other
  where
    rec = fromType

    fromTyTuple loc ts = foldl (\z a -> HM.appT loc z a) cons ts
      where
        cons = HM.varT loc $ mappend "Tuple" (showt $ length ts)

fromLit :: H.Literal Loc -> ParseResult Prim
fromLit x = case x of
  H.String loc val _     -> return $ PrimString (fromString val)
  H.Int loc val _        -> return $ PrimInt (fromInteger val)
  H.PrimInt loc val _    -> return $ PrimInt (fromInteger val)
  H.PrimString loc val _ -> return $ PrimString (fromString val)
  -- TODO FIXME: untyped literal numbers inconsistency.
  -- Here when we parse we also assign the type, but type is undefined here
  -- it can be any number. Not only double
  H.PrimFloat loc val _  -> floatsNotSupported
  H.PrimDouble loc val _ -> floatsNotSupported
  H.Frac loc val _       -> floatsNotSupported
  other                  -> parseFailedBy "Failed to parse literal" other
  where
    floatsNotSupported = parseFailedBy "floatsNotSupported" x



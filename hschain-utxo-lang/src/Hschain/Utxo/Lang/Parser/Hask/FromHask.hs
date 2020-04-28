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
import Data.Maybe
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
  H.Var loc qname -> fmap (Fix . Var loc) $ fromQName qname
  H.App loc a b -> liftA2 (\x y -> Fix $ Apply loc x y) (rec a) (rec b)
  H.Lambda loc ps body -> case ps of
    [p] -> fromLam loc p body
    _   -> fromLamList loc ps body
  H.Let loc binds exp -> liftA2 (\x y -> fromBgs y x) (fromBinds topExp binds) (rec exp)
  H.ExpTypeSig loc exp ty -> liftA2 (\x y -> Fix $ Ascr loc x (HM.typeToSignature y)) (rec exp) (fromType ty)
  H.Lit loc lit -> fmap (Fix . PrimE loc) $ fromLit lit
  H.If loc a b c -> liftA3 (\x y z -> Fix $ If loc x y z) (rec a) (rec b) (rec c)
  H.Tuple loc H.Boxed es -> fmap (Fix . Tuple loc . V.fromList) (mapM rec es)
  H.Con loc qname -> do
    n <- fromQName qname
    let bool b = Fix $ PrimE loc $ PrimBool b
    return $ case varName'name n of
      "True"  -> bool True
      "False" -> bool False
      _       -> Fix $ Cons loc (varToConsName n) V.empty
  H.List loc es -> fmap (Fix . VecE loc . NewVec loc . V.fromList) (mapM rec es)
  H.InfixApp loc a op b -> fromInfixApp loc op a b
  H.Paren _ exp         -> rec exp
  H.LeftSection loc a op  -> rec $ unfoldLeftSection loc a op
  H.RightSection loc op a -> rec $ unfoldRightSection loc op a
  H.Case loc expr alts -> liftA2 (fromCase loc) (rec expr) (mapM fromCaseAlt alts)
  H.RecConstr loc name fields -> liftA2 (fromRecConstr loc) (fromQName name) (mapM fromField fields)
  H.RecUpdate loc exp fields -> liftA2 (fromRecUpdate loc) (rec exp) (mapM fromField fields)
  other                 -> parseFailedBy "Failed to parse expression" other
  where
    rec = fromHaskExp

    unfoldLeftSection loc a op = H.Lambda loc [H.PVar loc x] (H.InfixApp loc a op (H.Var loc $ H.UnQual loc x))
      where x = H.Ident loc "x"

    unfoldRightSection loc op a = H.Lambda loc [H.PVar loc x] (H.InfixApp loc (H.Var loc $ H.UnQual loc x) op a)
      where x = H.Ident loc "x"

    fromLam loc p body = liftA2  (\x y -> Fix $ Lam loc x y) (fromPat p) (rec body)
    fromLamList loc ps body = liftA2  (\x y -> Fix $ LamList loc x y) (mapM fromPat ps) (rec body)

    fromInfixApp loc op a b =
      liftA3 (\x v y -> Fix $ InfixApply loc x v y) (rec a) (fromOp op) (rec b)

    fromOp = \case
      H.QVarOp loc qname -> fromQName qname
      other              -> parseFailedBy "Failed to parse infix application" other

    fromCase loc expr alts = Fix $ CaseOf loc expr alts

    fromRecConstr loc name fields = Fix $ RecConstr loc (varToConsName name) fields
    fromRecUpdate loc expr fields = Fix $ RecUpdate loc expr fields

    fromField = \case
      H.FieldUpdate loc name exp -> liftA2 (,) (fromQName name) (rec exp)
      _                          ->  parseFailedBy "Failed to parse field" topExp

    fromCaseAlt :: H.Alt Loc -> ParseResult (CaseExpr Lang)
    fromCaseAlt (H.Alt loc pat rhs mBinds)
      | noBinds mBinds = liftA2 CaseExpr (fromPat pat) (fromRhs rhs)
      | otherwise      = bindsNotSupported
      where
        bindsNotSupported = parseFailedBy "Binds for case expressions are not supported" topExp
        guardsNotSupported = parseFailedBy "Guards for case expressions are not supported" topExp
        noBinds = isNothing

        fromRhs rhs = case rhs of
          H.UnGuardedRhs _ expr -> fromHaskExp expr
          H.GuardedRhss _ _ -> guardsNotSupported




fromHaskModule :: H.Module Loc -> ParseResult Module
fromHaskModule = \case
  H.Module loc Nothing [] [] decls -> fromDecls loc decls
  _                                -> err
  where
    err = ParseFailed (H.fromSrcInfo noLoc) "Failed to parse module"

fromHaskDecl :: H.Decl Loc -> ParseResult (BindGroup Lang)
fromHaskDecl d = toBindGroup . return =<< toDecl d

fromDecls :: Loc -> [H.Decl Loc] -> ParseResult Module
fromDecls loc ds = do
  decls <- mapM toDecl ds
  bg <- toBindGroup decls
  return $ Module loc (toUserTypes decls) bg

toUserTypes :: [Decl] -> UserTypeCtx
toUserTypes ds =
  setupRecConstrs $ (\ts -> UserTypeCtx ts mempty) $ M.fromList $ fmap (\x -> (userType'name x, x)) $ mapMaybe getTypeDecl ds
  where
    getTypeDecl = \case
      DataDecl userType -> Just userType
      _                 -> Nothing

toDecl :: H.Decl Loc -> ParseResult Decl
toDecl x = case x of
  H.TypeSig loc names ty -> fmap (TypeSig loc (fmap toName names)) (fromQualType ty)
  H.FunBind loc matches  -> fmap (FunDecl loc) $ mapM fromMatch matches
  H.PatBind loc pat rhs mBinds -> fromPatBind x loc pat rhs mBinds
  H.DataDecl loc dataOrNew mCtx declHead cons mDeriving -> fromDataDecl loc dataOrNew mCtx declHead cons mDeriving
  other -> parseFailedBy "Unexpeceted declaration" other
  where
    fromPatBind m loc pat rhs mBinds = liftA2 (\name alt -> FunDecl loc [(name, alt)])
        (getPatName pat)
        (liftA2 (toAlt []) (fromRhs rhs) (mapM (fromBinds m) mBinds))

    fromMatch = \case
      m@(H.Match loc name pats rhs mBinds) -> fmap (toName name, ) (liftA3 toAlt (mapM fromPat pats) (fromRhs rhs) (mapM (fromBinds m) mBinds))
      other                                -> parseFailedBy "Failed to parse function bind" other

    toAlt pats rhs mBinds = pure $ Alt pats rhs mBinds

    fromRhs x = case x of
      H.UnGuardedRhs _ exp   -> fmap UnguardedRhs $ fromHaskExp exp
      H.GuardedRhss _ guards -> fmap GuardedRhs $ mapM (fromGuard x) guards

    fromGuard topExpr (H.GuardedRhs _ stmts expr) = case stmts of
      [stmt] -> liftA2 Guard (fromGuardStmt stmt) (fromHaskExp expr)
      _      -> parseFailedBy "Multiple statements are not supported" topExpr
      where
        fromGuardStmt = \case
          H.Qualifier _ expr -> fromHaskExp expr
          _                  -> parseFailedBy "Not supported type of statement in guard" topExpr

    getPatName = \case
      H.PVar loc name -> return $ toName name
      other           -> parseFailedBy "Failed to parse synonym name" other

    fromDataDecl loc dataOrNew mCtx declHead cons mDeriving =
      case dataOrNew of
        H.DataType _ -> case mDeriving of
          [] -> case mCtx of
            Just _ -> parseFailedBy "Context declaration for type is not supported" x
            Nothing -> getDataDecl declHead cons
          _ -> parseFailedBy "Deriving declaration is not supported" x
        H.NewType _ -> parseFailedBy "Newtype declaration is not supported" x

    getDataDecl declHead cons =
      liftA3 (\name args cases -> DataDecl $ UserType name args cases)
        (getName declHead)
        (pure $ getArgs declHead)
        (getCases cons)
      where
        getName = \case
          H.DHead loc name -> return $ toName name
          H.DHInfix loc _ _ -> parseFailedBy "Infix data declarations are not allowed" x
          H.DHParen _ a -> getName a
          H.DHApp _ f _ -> getName f

        getArgs = reverse . go []
          where
            go res = \case
              H.DHead _ _ -> res
              H.DHInfix _ _ _ -> res
              H.DHParen _ a -> go res a
              H.DHApp _ f a -> go (fromTyVarBind a : res) f

        fromTyVarBind = toName . \case
          H.KindedVar _ name _ -> name
          H.UnkindedVar _ name -> name

        getCases xs = fmap M.fromList $ mapM getCase xs

        getCase (H.QualConDecl _ _ _ conDecl) = case conDecl of
          H.ConDecl loc name args -> fromConDecl loc name args
          H.RecDecl loc name args -> fromRecDecl loc name args
          H.InfixConDecl _ _ _ _ -> parseFailedBy "Infix type declarations are not supported" x

        fromConDecl loc name args =
          fmap (\tys -> (varToConsName $ toName name, ConsDef $ V.fromList tys)) $ mapM fromType args

        fromRecDecl loc name args =
          fmap (\fields -> (resName, RecordCons $ V.fromList $ concat fields)) $ mapM fromField args
          where
            resName = varToConsName $ toName name

        fromField (H.FieldDecl loc names ty) = do
          resTy <- fromType ty
          let resNames = fmap toName names
          return $ fmap (\name -> RecordField name resTy) resNames


fromPat :: H.Pat Loc -> ParseResult Pat
fromPat topPat = case topPat of
  H.PVar loc name -> return $ PVar loc (toName name)
  H.PLit loc _ lit -> fmap (PPrim loc) (fromLit lit)
  H.PApp loc name ps -> do
      cons <- toConsName name
      case consName'name cons of
        "True"  -> onEmpty cons ps $ PPrim loc (PrimBool True)
        "False" -> onEmpty cons ps $ PPrim loc (PrimBool False)
        _       -> fmap (PCons loc cons) (mapM fromPat ps)
  H.PTuple loc _ ps -> fmap (PTuple loc) (mapM fromPat ps)
  H.PParen _ x    -> fromPat x
  H.PWildCard loc -> return $ PWildCard loc
  other           -> parseFailedBy "Failed to parse patter" other
  where
    onEmpty ConsName{..} ps x
      | null ps   = return x
      | otherwise = parseFailedBy (mconcat ["Constant pattern ", T.unpack consName'name, " should have no arguments"]) topPat

fromBgs :: Lang -> BindGroup Lang -> Lang
fromBgs rhs bgs = Fix $ Let (HM.getLoc rhs) bgs rhs

fromBinds :: (H.Annotated f, H.Pretty (f Loc)) => f Loc -> H.Binds Loc -> ParseResult (BindGroup Lang)
fromBinds topExp = \case
  H.BDecls _ decls -> toBindGroup =<< mapM toDecl decls
  _                -> parseFailedBy "Failed to parse binding group for expression" topExp

fromPatToVar :: H.Pat Loc -> ParseResult VarName
fromPatToVar = \case
  H.PVar _ name -> return $ toName name
  other         -> parseFailedBy "Failed to parse patter" other

varToConsName :: VarName -> ConsName
varToConsName VarName{..} = ConsName varName'loc varName'name

toConsName :: H.QName Loc -> ParseResult ConsName
toConsName = fmap varToConsName . fromQName

toName :: H.Name Loc -> VarName
toName = \case
  H.Ident  loc name -> VarName loc (fromString name)
  H.Symbol loc name -> VarName loc (fromString name)

fromQName :: H.QName Loc -> ParseResult VarName
fromQName = \case
  H.UnQual loc name -> return $ toName name
  other             -> parseFailedBy "Unexpected name" other

fromQualType :: H.Type Loc -> ParseResult Signature
fromQualType x = case x of
  H.TyForall loc Nothing mContext ty -> err
  ty -> fmap HM.typeToSignature $ fromType ty
  where
    err = parseFailedBy "Contexts are not allowed" x

fromType :: H.Type Loc -> ParseResult Type
fromType = \case
  H.TyFun loc a b -> liftA2 HM.arrowT (rec a) (rec b)
  H.TyTuple loc H.Boxed ts -> fmap (fromTyTuple loc) (mapM rec ts)
  H.TyApp loc a b -> do
    (name, args) <- getTyApp loc a b
    liftA2 HM.conT (fromQName name) (mapM rec args)
  H.TyVar _ name -> return $ HM.varT $ toName name
  H.TyCon _ name -> fmap (\v -> HM.conT v []) $ fromQName name
  H.TyParen loc ty -> rec ty
  H.TyKind _ ty _ -> rec ty
  other -> parseFailedBy  "Failed to parse type for" other
  where
    rec = fromType

    fromTyTuple loc ts = HM.conT cons ts
      where
        cons = VarName loc $ mappend "Tuple" (showt $ length ts)

    getTyApp loc a b = go a [b]
      where
        go a res = case a of
          H.TyApp _ a1 b1 -> go a1 (b1 : res)
          H.TyCon _ name  -> return (name, res)
          H.TyParen _ ty  -> go ty res
          other           -> parseFailedBy "Failed to parse type for" other

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


{-# OPTIONS_GHC -Wno-orphans #-}
module Hschain.Utxo.Lang.Parser.Hask.FromHask(
    fromHaskExp
  , fromHaskModule
  , toDecl
) where


import Control.Applicative

import Data.Fix
import Data.Maybe
import Data.String
import Data.Text (Text)

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import HSChain.Crypto (decodeBase58)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Exec.Module (toUserTypeCtx)
import Hschain.Utxo.Lang.Core.Types (Prim(..))
import Hschain.Utxo.Lang.Parser.Hask.Dependencies
import Hschain.Utxo.Lang.Parser.Hask.Utils

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Type.Check.HM as HM
import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Pretty as H

instance Alternative ParseResult where
  a <|> b = case a of
    ParseOk _       -> a
    ParseFailed _ _ -> b

  empty = ParseFailed H.noLoc ""

fromHaskExp :: H.Exp Loc -> ParseResult Lang
fromHaskExp topExp = case topExp of
  -- special hack for haskell quasi-quoter
  -- we parse $(a) as anti-quote to inline external haskell values
  H.SpliceExp _ (H.ParenSplice _ (H.Var loc qname)) -> fmap (Fix . AntiQuote loc Nothing) $ fromQName qname
  H.ExpTypeSig _ (H.SpliceExp _ (H.ParenSplice _ (H.Var loc qname))) tyExpr | Just ty <- toArgTypeName tyExpr ->
    fmap (Fix . AntiQuote loc (Just ty)) $ fromQName qname
  ---
  H.Var loc qname -> fmap (Fix . Var loc) $ fromQName qname
  -- special hack-case for bytestring literals
  -- we parse expressions @bytes "string"@ as ByteString decode from Base58
  H.App loc (H.Var _ qname) b | isBytesName qname ->
    case getStringLiteral b of
      Just txt ->
        case decodeBase58 txt of
          Just bs -> return $ Fix $ PrimE loc $ PrimBytes bs
          Nothing -> parseFailedBy "Failed to decode string as bytes (use Base58 encoding)" b
      Nothing -> parseFailedBy "Argument of bytes key-word should be string literal" b
  H.App loc a b -> liftA2 (\x y -> Fix $ Apply loc x y) (rec a) (rec b)
  H.Lambda loc ps body -> case ps of
    [p] -> fromLam loc p body
    _   -> fromLamList loc ps body
  H.Let _ binds expr -> liftA2 (\x y -> fromBgs y x) (fromLetBinds topExp binds) (rec expr)
  H.ExpTypeSig loc expr ty -> liftA2 (\x y -> Fix $ Ascr loc x (HM.typeToSignature y)) (rec expr) (fromType ty)
  H.Lit loc lit -> fmap (Fix . PrimE loc) $ fromLit lit
  H.If loc a b c -> liftA3 (\x y z -> Fix $ If loc x y z) (rec a) (rec b) (rec c)
  H.Tuple loc H.Boxed es -> fmap (Fix . Tuple loc . V.fromList) (mapM rec es)
  H.Con loc (H.Special _ (H.UnitCon _)) -> pure $ Fix $ Tuple loc V.empty
  H.Con loc qname -> do
    n <- fromQName qname
    let bool b = Fix $ PrimE loc $ PrimBool b
    return $ case varName'name n of
      "True"  -> bool True
      "False" -> bool False
      _       -> Fix $ Cons loc (varToConsName n) V.empty
  H.List loc es -> fmap (Fix . List loc . V.fromList) (mapM rec es)
  H.InfixApp loc a op b -> fromInfixApp loc op a b
  H.Paren _ expr        -> rec expr
  H.LeftSection loc a op  -> rec $ unfoldLeftSection loc a op
  H.RightSection loc op a -> rec $ unfoldRightSection loc op a
  H.Case loc expr alts -> liftA2 (fromCase loc) (rec expr) (mapM fromCaseAlt alts)
  H.RecConstr loc name fields -> liftA2 (fromRecConstr loc) (fromQName name) (mapM fromField fields)
  H.RecUpdate loc expr fields -> liftA2 (fromRecUpdate loc) (rec expr) (mapM fromField fields)
  H.NegApp loc expr -> fmap (Fix . NegApp loc) (rec expr)
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
      H.QVarOp _ qname -> fromQName qname
      other            -> parseFailedBy "Failed to parse infix application" other

    fromCase loc expr alts = Fix $ CaseOf loc expr alts

    fromRecConstr loc name fields = Fix $ RecConstr loc (varToConsName name) fields
    fromRecUpdate loc expr fields = Fix $ RecUpdate loc expr fields

    fromField = \case
      H.FieldUpdate _ name expr -> liftA2 (,) (fromQName name) (rec expr)
      _                        ->  parseFailedBy "Failed to parse field" topExp

    fromCaseAlt :: H.Alt Loc -> ParseResult (CaseExpr Lang)
    fromCaseAlt (H.Alt _ pat rhs mBinds)
      | noBinds mBinds = liftA2 CaseExpr (fromPat pat) (fromRhs rhs)
      | otherwise      = bindsNotSupported
      where
        bindsNotSupported = parseFailedBy "Binds for case expressions are not supported" topExp
        guardsNotSupported = parseFailedBy "Guards for case expressions are not supported" topExp
        noBinds = isNothing

        fromRhs = \case
          H.UnGuardedRhs _ expr -> fromHaskExp expr
          H.GuardedRhss _ _ -> guardsNotSupported

fromHaskModule :: H.Module Loc -> ParseResult Module
fromHaskModule = \case
  H.Module loc Nothing [] [] decls -> fromDecls loc decls
  _                                -> err
  where
    err = ParseFailed (H.fromSrcInfo noLoc) "Failed to parse module"

fromDecls :: Loc -> [H.Decl Loc] -> ParseResult Module
fromDecls loc ds = do
  decls <- mapM toDecl ds
  bg <- toBindGroup decls
  return $ Module
    { module'loc       = loc
    , module'userTypes = toUserTypes decls
    , module'binds     = bg
    }

toUserTypes :: [Decl] -> UserTypeCtx
toUserTypes ds =
  toUserTypeCtx $ mapMaybe getTypeDecl ds
  where
    getTypeDecl = \case
      DataDecl userType -> Just userType
      _                 -> Nothing

fromDeclBinds :: H.Decl Loc -> H.Binds Loc -> ParseResult (Binds Lang)
fromDeclBinds = fromLetBinds

toDecl :: H.Decl Loc -> ParseResult Decl
toDecl x = case x of
  H.TypeSig loc names ty -> fmap (TypeSig loc (fmap toName names)) (fromQualType ty)
  H.FunBind loc matches  -> fmap (FunDecl loc) $ mapM fromMatch matches
  H.PatBind loc pat rhs mBinds -> fromPatBindName x loc pat rhs mBinds <|> fromPatBind x loc pat rhs mBinds
  H.DataDecl loc dataOrNew mCtx declHead cons mDeriving -> fromDataDecl loc dataOrNew mCtx declHead cons mDeriving
  other -> parseFailedBy "Unexpeceted declaration" other
  where
    fromPatBindName m loc pat rhs mBinds = liftA2 (\name alt -> FunDecl loc [(name, alt)])
        (getPatName pat)
        (fmap pure $ liftA2 (toAlt []) (fromRhs rhs) (mapM (fromDeclBinds m) mBinds))

    fromPatBind m loc pat rhs mBinds = liftA2 (PatDecl loc)
        (fromPat pat)
        (liftA2 (toAlt []) (fromRhs rhs) (mapM (fromDeclBinds m) mBinds))

    fromMatch = \case
      m@(H.Match _ name pats rhs mBinds) -> fmap ((toName name, ) . pure) (liftA3 toAlt (mapM fromPat pats) (fromRhs rhs) (mapM (fromLetBinds m) mBinds))
      other                              -> parseFailedBy "Failed to parse function bind" other

    toAlt pats rhs mBinds = Alt pats rhs mBinds

    fromRhs = \case
      H.UnGuardedRhs _ expr  -> fmap UnguardedRhs $ fromHaskExp expr
      H.GuardedRhss _ guards -> fmap GuardedRhs $ mapM (fromGuard x) guards

    fromGuard topExpr (H.GuardedRhs _ stmts expr) = case stmts of
      [stmt] -> liftA2 Guard (fromGuardStmt stmt) (fromHaskExp expr)
      _      -> parseFailedBy "Multiple statements are not supported" topExpr
      where
        fromGuardStmt = \case
          H.Qualifier _ e    -> fromHaskExp e
          _                  -> parseFailedBy "Not supported type of statement in guard" topExpr

    getPatName = \case
      H.PVar _ name -> return $ toName name
      other         -> parseFailedBy "Failed to parse synonym name" other

    fromDataDecl _ dataOrNew mCtx declHead cons mDeriving =
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
          H.DHead _ name -> return $ toName name
          H.DHInfix _ _ _ -> parseFailedBy "Infix data declarations are not allowed" x
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

        fromConDecl _ name args =
          fmap (\tys -> (varToConsName $ toName name, ConsDef $ V.fromList tys)) $ mapM fromType args

        fromRecDecl _ name args =
          fmap (\fields -> (resName, RecordCons $ V.fromList $ concat fields)) $ mapM fromField args
          where
            resName = varToConsName $ toName name

        fromField (H.FieldDecl _ names ty) = do
          resTy <- fromType ty
          let resNames = fmap toName names
          return $ fmap (\name -> RecordField name resTy) resNames


fromPat :: H.Pat Loc -> ParseResult Pat
fromPat topPat = case topPat of
  H.PVar loc name -> return $ PVar loc (toName name)
  H.PLit loc _ lit -> fmap (PPrim loc) (fromLit lit)
  H.PApp loc (H.Special _ (H.UnitCon _)) [] -> pure $ PTuple loc []
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

fromBgs :: Lang -> Binds Lang -> Lang
fromBgs rhs bgs = Fix $ Let (HM.getLoc rhs) bgs rhs

fromLetBinds :: (H.Annotated f, H.Pretty (f Loc)) => f Loc -> H.Binds Loc -> ParseResult (Binds Lang)
fromLetBinds topExp = \case
  H.BDecls _ decls -> toBindGroup =<< mapM toDecl decls
  _                -> parseFailedBy "Failed to parse binding group for expression" topExp

toConsName :: H.QName Loc -> ParseResult ConsName
toConsName = fmap varToConsName . fromQName

toName :: H.Name Loc -> VarName
toName = \case
  H.Ident  loc name -> VarName loc (fromString name)
  H.Symbol loc name -> VarName loc (fromString name)

fromQName :: H.QName Loc -> ParseResult VarName
fromQName = \case
  H.UnQual _ name -> return $ toName name
  other           -> parseFailedBy "Unexpected name" other

fromQualType :: H.Type Loc -> ParseResult Signature
fromQualType x = case x of
  H.TyForall _ Nothing _mContext _ty -> err
  ty -> fmap HM.typeToSignature $ fromType ty
  where
    err = parseFailedBy "Contexts are not allowed" x

fromType :: H.Type Loc -> ParseResult Type
fromType = \case
  H.TyFun loc a b -> liftA2 (HM.arrowT loc) (rec a) (rec b)
  H.TyTuple loc H.Boxed ts -> fmap (fromTyTuple loc) (mapM rec ts)
  H.TyList loc a -> fmap (HM.listT loc) (rec a)
  H.TyApp loc a b -> do
    (name, args) <- getTyApp loc a b
    v <- fromQName name
    fmap (HM.conT (varName'loc v) (varName'name v)) (mapM rec args)
  H.TyVar _   name -> return $ (\VarName{..} -> HM.varT varName'loc varName'name) $ toName name
  H.TyCon loc name -> fmap (\v -> HM.conT loc (varName'name v) []) $ fromQName name
  H.TyParen _ ty   -> rec ty
  H.TyKind  _ ty _ -> rec ty
  other -> parseFailedBy  "Failed to parse type for" other
  where
    rec = fromType

    fromTyTuple loc ts = HM.tupleT loc ts

    getTyApp _ a b = go a [b]
      where
        go x res = case x of
          H.TyApp _ a1 b1 -> go a1 (b1 : res)
          H.TyCon _ name  -> return (name, res)
          H.TyParen _ ty  -> go ty res
          other           -> parseFailedBy "Failed to parse type for" other

fromLit :: H.Literal Loc -> ParseResult Prim
fromLit x = case x of
  H.String     _ val _ -> return $ PrimText (fromString val)
  H.Int        _ val _ -> return $ PrimInt (fromInteger val)
  H.PrimInt    _ val _ -> return $ PrimInt (fromInteger val)
  H.PrimString _ val _ -> return $ PrimText (fromString val)
  -- TODO FIXME: untyped literal numbers inconsistency.
  -- Here when we parse we also assign the type, but type is undefined here
  -- it can be any number. Not only double
  H.PrimFloat{}        -> floatsNotSupported
  H.PrimDouble{}       -> floatsNotSupported
  H.Frac{}             -> floatsNotSupported
  other                -> parseFailedBy "Failed to parse literal" other
  where
    floatsNotSupported = parseFailedBy "floatsNotSupported" x


isBytesName :: H.QName Loc -> Bool
isBytesName = \case
  H.UnQual _ (H.Ident _ "bytes") -> True
  _                              -> False

getStringLiteral :: H.Exp Loc -> Maybe Text
getStringLiteral = \case
  H.Lit _ (H.String _ val _) -> Just $ T.pack val
  _                          -> Nothing

toArgTypeName :: H.Type Loc -> Maybe QuoteType
toArgTypeName expr =
  case expr of
    H.TyCon _ tyName -> case tyName of
      H.UnQual _ (H.Ident _ name) -> case name of
        "Int"       -> Just IntQ
        "Bool"      -> Just BoolQ
        "Text"      -> Just TextQ
        "Bytes"     -> Just BytesQ
        "Sigma"     -> Just SigmaQ
        "Script"    -> Just ScriptQ
        "PublicKey" -> Just PublicKeyQ
        _           -> err
      _ -> err
    H.TyList _ t     -> fmap ListQ $ toArgTypeName t
    H.TyTuple _ _ ts -> fmap TupleQ $ mapM toArgTypeName ts
    _                -> err
  where
    err = Nothing

-- | Module for functions that convert Haskell expressions to expressions of our language.
module Hschain.Utxo.Lang.Parser.Hask.ToHask(
    toHaskExp
  , toHaskModule
  , toHaskType
) where

import Data.ByteString (ByteString)
import Data.Fix

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types

import qualified Data.Text as T
import qualified Data.Vector as V

import HSChain.Crypto.Classes (encodeBase58)
import qualified Language.Haskell.Exts.Syntax as H

import Type.Check.HM.Type() -- import instances

import qualified Type.Check.HM as HM

import qualified Hschain.Utxo.Lang.Const as Const

toHaskExp :: Lang -> H.Exp Loc
toHaskExp (Fix expr) = case expr of
  Var loc name -> toVar loc name
  Apply loc a b -> H.App loc (rec a) (rec b)
  InfixApply loc a v b -> H.InfixApp loc (rec a) (H.QVarOp (HM.getLoc v) $ toSymbolQName v) (rec b)
  Lam loc name a -> H.Lambda loc [toPat name] (rec a)
  LamList loc vs a -> H.Lambda loc (fmap toPat vs) (rec a)
  Ascr loc a ty -> H.ExpTypeSig loc (rec a) (toType ty)
  -- case
  Cons loc name args -> foldl (\f z -> H.App loc f z) (toCon loc name) $ fmap rec args
  CaseOf loc e alts -> H.Case loc (rec e) (fmap (toCaseAlt loc) alts)
  -- records
  RecConstr loc cons fields -> H.RecConstr loc (toQName $ consToVarName cons) $ fmap toRecField fields
  RecUpdate loc e fields -> H.RecUpdate loc (rec e) $ fmap toRecField fields
  -- primitives
  PrimE loc p -> toLiteral loc p
  -- logic
  If loc a b c -> H.If loc (rec a) (rec b) (rec c)
  -- tuples
  Tuple loc ts -> H.Tuple loc H.Boxed (fmap rec $ V.toList ts)
  List loc ts -> H.List loc (fmap rec $ V.toList ts)
  NegApp loc a -> H.NegApp loc (rec a)
  FailCase loc -> H.Var loc (H.UnQual loc $ H.Ident loc "undefined")
  Let loc binds e -> H.Let loc (toLetBinds loc binds) (rec e)
  PrimLet loc binds e -> H.Let loc (toPrimLetBinds loc binds) (rec e)
  AltE _ _ _ -> error "Alt is for internal usage"
  AntiQuote loc mty name -> case mty of
    Just ty -> H.ExpTypeSig loc (H.SpliceExp loc (H.ParenSplice loc (toVar loc name))) (fromQuoteType loc ty)
    Nothing -> H.SpliceExp loc (H.ParenSplice loc (toVar loc name))
  where
    rec = toHaskExp
    toLetBinds loc bg = H.BDecls loc $ toDecl bg
    toPrimLetBinds loc bg = H.BDecls loc $ toPrimDecl bg

    toRecField (name, e) = H.FieldUpdate (HM.getLoc name) (toQName name) (rec e)

    toCon loc = H.Con loc . toQName . consToVarName

    toCaseAlt loc CaseExpr{..} = H.Alt loc (toPat caseExpr'lhs) (toRhs caseExpr'rhs) Nothing
      where
        toRhs = H.UnGuardedRhs loc . rec

toLiteral :: Loc -> Prim -> H.Exp Loc
toLiteral loc = \case
  PrimInt x -> lit $ H.Int loc (fromIntegral x) (show x)
  PrimString x -> toText x
  PrimBool x -> H.Con loc $ bool loc x
  PrimSigma x -> sigma loc x
  PrimBytes x -> H.App loc (H.Var loc $ toQName $ VarName loc "pack58") (toText (encodeBase58 x))
  where
    toText x = lit $ H.String loc (T.unpack x) (T.unpack x)
    lit = H.Lit loc

    sigma :: Loc -> Sigma ByteString -> H.Exp Loc
    sigma src x = foldFix go x
      where
        go = \case
          SigmaPk pkey -> let keyTxt = encodeBase58 pkey
                            in  ap (VarName src "pk") $ lit $ H.String src (T.unpack keyTxt) (T.unpack keyTxt)
          SigmaAnd as  -> foldl1 (ap2 (VarName src Const.sigmaAnd)) as
          SigmaOr  as  -> foldl1 (ap2 (VarName src Const.sigmaOr)) as
          SigmaBool b  -> H.Con src $ bool src b

        ap f a = H.App (HM.getLoc f) (toVar (HM.getLoc f) f) a
        ap2 f a b = H.App src (H.App src (toVar src f) a) b

-- | TODO implement rendering of type declarations
toHaskModule :: Module -> H.Module Loc
toHaskModule (Module loc _ bs) = H.Module loc Nothing [] [] (toDecl bs)

toDecl :: [Bind Lang] -> [H.Decl Loc]
toDecl bs = toBind =<< bs
  where
    toBind Bind{..} = case bind'type of
      Nothing -> fmap (\alt -> H.FunBind (HM.getLoc bind'name) $ pure $ toMatch bind'name alt) bind'alts
      Just ty ->
        let signature = H.TypeSig tyLoc [H.Ident tyLoc (T.unpack $ varName'name bind'name)] (toType ty)
            funBinds = fmap (\alt -> H.FunBind (HM.getLoc bind'name) $ pure $ toMatch bind'name alt) bind'alts

            tyLoc :: Loc
            tyLoc = HM.getLoc ty
        in  signature : funBinds

    toLetBinds loc bg = H.BDecls loc $ toDecl bg

    toMatch :: VarName -> Alt Lang -> H.Match Loc
    toMatch name alt = H.Match (HM.getLoc name) (toIdentName name) (toPats alt) (toRhs alt) (fmap (toLetBinds (HM.getLoc name)) $ alt'where alt)

    toPats :: Alt a -> [H.Pat Loc]
    toPats = fmap toPat . alt'pats


    toRhs :: Alt Lang -> H.Rhs Loc
    toRhs Alt{..} = case alt'expr of
      UnguardedRhs rhs  -> H.UnGuardedRhs (HM.getLoc alt'expr) (toHaskExp rhs)
      GuardedRhs guards -> H.GuardedRhss (HM.getLoc alt'expr) (fmap toGuard guards)

    toGuard :: Guard Lang -> H.GuardedRhs Loc
    toGuard Guard{..} =
      H.GuardedRhs
        (HM.getLoc guard'predicate)
        [H.Qualifier (HM.getLoc guard'predicate) $ toHaskExp guard'predicate]
        (toHaskExp guard'rhs)

toPrimDecl :: [(VarName, Lang)] -> [H.Decl Loc]
toPrimDecl = fmap toBind
  where
    toBind (name, expr) = H.FunBind (HM.getLoc name) $ [toMatch name expr]

    toMatch name expr = H.Match (HM.getLoc name) (toIdentName name) [] (H.UnGuardedRhs (HM.getLoc expr) $ toHaskExp expr) Nothing

toPat :: Pat -> H.Pat Loc
toPat pat = case pat of
  PVar _ var -> toPVar var
  PPrim loc p -> toLit loc p
  PCons loc name args -> H.PApp loc (toQName $ consToVarName name) $ fmap toPat args
  PTuple loc args -> H.PTuple loc H.Boxed (fmap toPat args)
  PWildCard loc -> H.PWildCard loc
  where
    toLit loc p = case p of
      PrimInt x -> lit loc $ H.Int loc (fromIntegral x) (show x)
      PrimString x -> lit loc $ H.String loc (T.unpack x) (T.unpack x)
      PrimBool x -> H.PApp loc (bool loc x) []
      _ -> error "Failed to convert literal"

    toPVar var = H.PVar (varName'loc var) (toIdentName var)

    lit loc = H.PLit loc (H.Signless loc)

bool :: Loc -> Bool -> H.QName Loc
bool loc x = H.UnQual loc $ H.Ident loc $ show x

toIdentName :: VarName -> H.Name Loc
toIdentName (VarName loc name) = H.Ident loc (T.unpack name)

toSymbolName :: VarName -> H.Name Loc
toSymbolName VarName{..} = H.Symbol varName'loc (T.unpack varName'name)


toType :: Signature -> H.Type Loc
toType x = case splitToPreds x of
  (_, ty) -> toHaskType ty
  where
    splitToPreds = foldFix go . HM.unSignature
      where
        go = \case
          HM.MonoT ty                  -> ([], ty)
          HM.ForAllT _ name (xs, ty) -> (name : xs, ty)

toHaskType :: Type -> H.Type Loc
toHaskType = foldFix go . HM.unType
  where
    go = \case
      HM.VarT loc var      -> H.TyVar loc (toIdentName $ VarName loc var)
      HM.ConT loc con args -> fromTyCon loc con args
      HM.ArrowT loc a b    -> H.TyFun loc a b
      HM.ListT loc a       -> H.TyList loc a
      HM.TupleT loc as     -> H.TyTuple loc H.Boxed as

    fromTyCon loc con args =
      foldl (\a b -> H.TyApp loc a b) (H.TyCon loc (toQName $ VarName loc con)) args

toQName :: VarName -> H.QName Loc
toQName x = H.UnQual (HM.getLoc x) $ toIdentName x

toSymbolQName :: VarName -> H.QName Loc
toSymbolQName x@(VarName loc _) = H.UnQual loc $ toSymbolName x

toVar :: Loc -> VarName -> H.Exp Loc
toVar loc name = H.Var loc (toQName name)

fromArgType :: Loc -> ArgType -> H.QName Loc
fromArgType loc ty = H.UnQual loc $ H.Ident loc $ T.unpack $ argTypeName ty

fromQuoteType :: Loc -> QuoteType -> H.Type Loc
fromQuoteType loc = \case
  PrimQ ty   -> H.TyCon loc $ fromArgType loc ty
  SigmaQ     -> primQ "Sigma"
  ScriptQ    -> primQ "Script"
  PublicKeyQ -> primQ "PublicKey"
  ListQ ty   -> H.TyList loc (fromQuoteType loc ty)
  TupleQ ts  -> H.TyTuple loc H.Boxed $ fmap (fromQuoteType loc) ts
  where
    primQ name = H.TyCon loc $ H.UnQual loc $ H.Ident loc name


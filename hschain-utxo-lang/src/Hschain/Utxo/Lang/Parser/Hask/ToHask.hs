-- | Module for functions that convert Haskell expressions to expressions of our language.
module Hschain.Utxo.Lang.Parser.Hask.ToHask(
    toHaskExp
  , toHaskModule
  , toHaskType
) where

import Hex.Common.Text (showt)

import Data.ByteString (ByteString)
import Data.Fix

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types

import qualified Data.Text as T
import qualified Data.Vector as V

import HSChain.Crypto.Classes (encodeBase58)
import qualified Language.Haskell.Exts.Syntax as H

import Language.HM.Type() -- import instances

import qualified Language.HM as HM

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
  -- operations
  UnOpE loc op a -> fromUnOp loc op a
  BinOpE loc op a b -> fromBimOp loc op a b
  -- environment
  GetEnv loc env -> fromEnv loc env
  -- sigmas
  SigmaE loc sigma -> fromSigma loc sigma
  -- vectors
  VecE loc vec -> fromVec loc vec
  -- text
  TextE loc txt -> fromText loc txt
  -- bytes
  BytesE loc bs -> fromBytes loc bs
  -- boxes
  BoxE loc box -> fromBox loc box
  -- debug
  Trace loc a b -> ap2 (VarName loc "trace") a b
  FailCase loc -> H.Var loc (H.UnQual loc $ H.Ident loc "undefined")
  Let loc binds e -> H.Let loc (toLetBinds loc binds) (rec e)
  PrimLet loc binds e -> H.Let loc (toPrimLetBinds loc binds) (rec e)
  CheckSig loc a b -> ap2 (VarName loc Const.checkSig) a b
  CheckMultiSig loc a b c -> ap3 (VarName loc Const.checkMultiSig) a b c
  AltE _ _ _ -> error "Alt is for internal usage"
  AntiQuote loc mty name -> case mty of
    Just ty -> H.Paren loc $ H.InfixApp loc (toVar loc name) (H.QVarOp loc $ H.UnQual loc $ H.Ident loc "#") (H.Con loc $ fromArgType loc ty)
    Nothing -> H.Paren loc (toVar loc name)
  where
    rec = toHaskExp
    ap f x = H.App (HM.getLoc f) (toVar (HM.getLoc f) f) (rec x)
    ap2 f x y = H.App (HM.getLoc y) (H.App (HM.getLoc f) (toVar (HM.getLoc f) f) (rec x)) (rec y)
    ap3 f x y z = H.App (HM.getLoc z) (H.App (HM.getLoc y) (H.App (HM.getLoc f) (toVar (HM.getLoc f) f) (rec x)) (rec y)) (rec z)
    toLetBinds loc bg = H.BDecls loc $ toDecl bg
    toPrimLetBinds loc bg = H.BDecls loc $ toPrimDecl bg

    toRecField (name, e) = H.FieldUpdate (HM.getLoc name) (toQName name) (rec e)

    toCon loc = H.Con loc . toQName . consToVarName

    toCaseAlt loc CaseExpr{..} = H.Alt loc (toPat caseExpr'lhs) (toRhs caseExpr'rhs) Nothing
      where
        toRhs = H.UnGuardedRhs loc . rec

    fromUnOp loc op a = case op of
      Not       -> ap (VarName loc "not") a
      Neg       -> H.NegApp loc (rec a)
      TupleAt size n -> ap2 (VarName loc $ mconcat ["tuple", showt size, "At"]) (Fix $ PrimE loc $ PrimInt $ fromIntegral n) a

    fromBimOp loc op = case op of
      And                   -> op2' "&&"
      Or                    -> op2' "||"
      Plus                  -> op2' "+"
      Minus                 -> op2' "-"
      Times                 -> op2' "*"
      Div                   -> op2' "/"
      Equals                -> op2' "=="
      NotEquals             -> op2' "/="
      LessThan              -> op2' "<"
      GreaterThan           -> op2' ">"
      LessThanEquals        -> op2' "<="
      GreaterThanEquals     -> op2' ">="
      where
        op2' name a b = op2 loc name a b

    fromEnv _ = \case
      Height loc     -> toVar loc (VarName loc Const.getHeight)
      Input loc a    -> ap (VarName loc "getInput") a
      Output loc a   -> ap (VarName loc "getOutput") a
      Self loc       -> toVar loc (VarName loc Const.getSelf)
      Inputs loc     -> toVar loc (VarName loc Const.getInputs)
      Outputs loc    -> toVar loc (VarName loc Const.getOutputs)
      DataInputs loc -> toVar loc (VarName loc Const.getDataInputs)
      GetVar loc ty  -> toVar loc (VarName loc $ getEnvVarName ty)

    fromSigma _ = \case
      Pk loc a        -> ap (VarName loc "pk") a
      SOr loc a b     -> op2 loc "|||" a b
      SAnd loc a b    -> op2 loc "&&&" a b
      SPrimBool loc a -> ap (VarName loc "toSigma") a


    fromVec _ = \case
      NewVec loc vs     -> H.List loc (fmap rec $ V.toList vs)
      VecAppend loc a b -> op2 loc "++" a b
      VecAt loc a b     -> op2 loc "!" a b
      VecLength loc     -> toVar loc (VarName loc "length")
      VecMap loc        -> toVar loc (VarName loc "map")
      VecFold loc       -> toVar loc (VarName loc Const.foldl)
      VecAndSigma loc   -> toVar loc (VarName loc "andSigma")
      VecOrSigma loc    -> toVar loc (VarName loc "orSigma")

    fromText _ = \case
      TextAppend loc a b    -> op2 loc "<>" a b
      ConvertToText loc tag -> toVar loc (VarName loc $ mconcat ["show", fromTextTag tag])
      TextLength loc        -> toVar loc (VarName loc "lengthText")
      where
        fromTextTag = \case
          IntToText    -> "Int"
          ScriptToText -> "Script"
          BoolToText   -> "Bool"

    fromBytes _ = \case
      BytesAppend loc a b            -> ap2 (VarName loc Const.appendBytes) a b
      BytesLength loc a              -> ap  (VarName loc $ Const.lengthBytes) a
      SerialiseToBytes loc tag a     -> ap  (VarName loc $ Const.serialiseBytes $ argTypeName tag) a
      DeserialiseFromBytes loc tag a -> ap  (VarName loc $ Const.deserialiseBytes $ argTypeName tag) a
      BytesHash loc algo a           -> case algo of
        Sha256     -> ap (VarName loc "sha256") a

    fromBox _ = \case
      BoxAt loc a field   -> fromBoxField loc a field

    fromBoxField loc a = \case
      BoxFieldId          -> get Const.getBoxId
      BoxFieldValue       -> get Const.getBoxValue
      BoxFieldScript      -> get Const.getBoxScript
      BoxFieldArgList ty  -> get (getBoxArgVar ty)
      BoxFieldPostHeight  -> get Const.getBoxPostHeight
      where
        get name = ap (VarName loc name) a

    op2 :: Loc -> String -> Lang -> Lang -> H.Exp Loc
    op2 loc name a b = H.InfixApp loc (rec a) (H.QVarOp loc $ H.UnQual loc $ H.Symbol loc name) (rec b)


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
    sigma src x = cata go x
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
    splitToPreds = cata go . HM.unSignature
      where
        go = \case
          HM.MonoT ty                  -> ([], ty)
          HM.ForAllT _ name (xs, ty) -> (name : xs, ty)

toHaskType :: Type -> H.Type Loc
toHaskType = cata go . HM.unType
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


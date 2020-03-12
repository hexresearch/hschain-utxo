module Hschain.Utxo.Lang.Parser.Hask.ToHask(
    toHaskExp
  , toHaskModule
) where

import Hex.Common.Text

import Control.Monad

import Data.Fix

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Language.Haskell.Exts.Pretty


import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Parser as H

import qualified Language.HM as HM


toHaskExp :: Lang -> H.Exp Loc
toHaskExp (Fix expr) = case expr of
  Var loc name -> toVar loc name
  Apply loc a b -> H.App loc (rec a) (rec b)
  InfixApply loc a v b -> H.InfixApp loc (rec a) (H.QVarOp (HM.getLoc v) $ toSymbolQName v) (rec b)
  Lam loc name a -> H.Lambda loc [H.PVar (HM.getLoc name) $ toIdentName name] (rec a)
  LamList loc vs a -> H.Lambda loc (fmap (\v -> H.PVar (HM.getLoc v) $ toIdentName v) vs) (rec a)
  Let loc bg a -> H.Let loc (toLetBinds loc bg) (toHaskExp a)
  LetRec loc name a b -> undefined
  Ascr loc a ty -> H.ExpTypeSig loc (rec a) (toType (HM.Signature $ Fix $ HM.MonoT ty))
  -- primitives
  PrimE loc p -> toLiteral loc p
  -- logic
  If loc a b c -> H.If loc (rec a) (rec b) (rec c)
  Pk loc a -> ap (VarName loc "pk") a
  -- tuples
  Tuple loc ts -> H.Tuple loc H.Boxed (fmap rec $ V.toList ts)
  -- operations
  UnOpE loc op a -> fromUnOp loc op a
  BinOpE loc op a b -> fromBimOp loc op a b
  -- environment
  GetEnv loc env -> fromEnv loc env
  -- vectors
  VecE loc vec -> fromVec loc vec
  -- text
  TextE loc txt -> fromText loc txt
  -- boxes
  BoxE loc box -> fromBox loc box
  -- undefined
  Undef loc -> toVar loc (VarName loc "undefined")
  -- debug
  Trace loc a b -> ap2 (VarName loc "trace") a b
  where
    rec = toHaskExp
    ap f x = H.App (HM.getLoc f) (toVar (HM.getLoc f) f) (rec x)
    ap2 f x y = H.App (HM.getLoc y) (H.App (HM.getLoc f) (toVar (HM.getLoc f) f) (rec x)) (rec y)
--    op2 f x y = H.InfixApp (getLoc f) (rec x) (H.QVarOp (getLoc f) $ toSymbolQName' f) (rec y)

    toLetBinds loc bg = H.BDecls loc $ toDecl bg

    fromUnOp loc op a = case op of
      Not       -> ap (VarName loc "not") a
      Neg       -> ap (VarName loc "negate") a
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
        op2' name a b = op2 loc name (rec a) (rec b)

    fromEnv _ = \case
      Height loc    -> toVar loc (VarName loc "getHeight")
      Input loc a   -> ap (VarName loc "getInput") a
      Output loc a  -> ap (VarName loc "getOutput") a
      Self loc      -> toVar loc (VarName loc "getSelf")
      Inputs loc    -> toVar loc (VarName loc "getInputs")
      Outputs loc   -> toVar loc (VarName loc "getOutputs")
      GetVar loc a  -> ap (VarName loc "getVar") a

    fromVec _ = \case
      NewVec loc vs     -> H.List loc (fmap rec $ V.toList vs)
      VecAppend loc a b -> op2 loc "++" (rec a) (rec b)
      VecAt loc a b     -> op2 loc "!" (rec a) (rec b)
      VecLength loc     -> toVar loc (VarName loc "length")
      VecMap loc        -> toVar loc (VarName loc "map")
      VecFold loc       -> toVar loc (VarName loc "fold")

    fromText _ = \case
      TextAppend loc a b  -> op2 loc "<>" (rec a) (rec b)
      ConvertToText tag loc   -> toVar loc (VarName loc $ mconcat ["show", fromTextTag tag])
      TextLength loc      -> toVar loc (VarName loc "lengthText")
      TextHash loc algo   -> case algo of
        Sha256     -> toVar loc (VarName loc "sha256")
        Blake2b256 -> toVar loc (VarName loc "blake2b256")
      where
        fromTextTag = \case
          IntToText    -> "Int"
          ScriptToText -> "Script"
          BoolToText   -> "Bool"

    fromBox :: Loc -> BoxExpr Lang -> H.Exp Loc
    fromBox _ = \case
      PrimBox loc box     -> fromPrimBox loc box
      BoxAt loc a field   -> fromBoxField loc a field

    fromPrimBox loc Box{..} = H.RecConstr loc (qname "Box")
      [ field "box'id"     $ prim $ PrimString $ unBoxId box'id
      , field "box'value"  $ prim $ PrimInt  $ box'value
      , field "box'script" $ prim $ PrimString $ unScript box'script
      , field "box'args"   $ Fix $ VecE loc $ NewVec loc (V.fromList $ fmap (\(a, b) -> Fix $ Tuple loc (V.fromList [a, b])) args)
      ]
      where
        qname a = toQName $ VarName loc a
        field name a = H.FieldUpdate loc (qname name) (rec a)
        prim = Fix . PrimE loc
        args = fmap (\(txt, val) -> (Fix $ PrimE loc $ PrimString txt, Fix $ PrimE loc val)) $ M.toList box'args

    fromBoxField loc a = \case
      BoxFieldId          -> get "getBoxId"
      BoxFieldValue       -> get "getBoxValue"
      BoxFieldScript      -> get "getBoxScript"
      BoxFieldArg b       -> ap2 (VarName loc "getBoxArg") b a
      where
        get name = ap (VarName loc name) a

op2 :: Loc -> String -> H.Exp Loc -> H.Exp Loc -> H.Exp Loc
op2 loc name a b = H.InfixApp loc a (H.QVarOp loc $ H.UnQual loc $ H.Symbol loc name) b


toLiteral :: Loc -> Prim -> H.Exp Loc
toLiteral loc = \case
  PrimInt x -> lit $ H.Int loc (fromIntegral x) (show x)
  PrimString x -> lit $ H.String loc (T.unpack x) (T.unpack x)
  PrimBool x -> H.Con loc $ bool loc x
  PrimSigma x -> sigma loc x
  where
    lit = H.Lit loc
    bool loc x = H.UnQual loc $ H.Ident loc $ show x

    sigma :: Loc -> Sigma PublicKey -> H.Exp Loc
    sigma loc x = cata go x
      where
        go :: SigmaExpr PublicKey (H.Exp Loc) -> H.Exp Loc
        go = \case
          SigmaPk pkey -> let keyTxt = publicKeyToText pkey
                            in  ap (VarName loc "pk") $ lit $ H.String loc (T.unpack keyTxt) (T.unpack keyTxt)
          SigmaAnd as  -> foldl1 (op2 loc "&&") as
          SigmaOr  as  -> foldl1 (op2 loc "||") as

        ap f x = H.App (HM.getLoc f) (toVar (HM.getLoc f) f) x

toHaskModule :: Module -> H.Module Loc
toHaskModule (Module loc bs) = H.Module loc Nothing [] [] (toDecl bs)

toDecl :: BindGroup Lang -> [H.Decl Loc]
toDecl bs = toBind =<< bs
  where
    toBind Bind{..} = case bind'type of
      Nothing -> return $ H.FunBind (HM.getLoc bind'name) $ pure $ toMatch bind'name bind'alt
      Just ty ->
        let signature = H.TypeSig tyLoc [H.Ident tyLoc (T.unpack $ varName'name bind'name)] (toType ty)
            funBind = H.FunBind (HM.getLoc bind'name) $ pure $ toMatch bind'name bind'alt
            tyLoc = HM.getLoc ty
        in  [ signature, funBind ]

    toMatch :: VarName -> Alt Lang -> H.Match Loc
    toMatch name alt = H.Match (HM.getLoc name) (toIdentName name) (toPats alt) (toRhs alt) Nothing

    toPats :: Alt a -> [H.Pat Loc]
    toPats = fmap toPat . alt'pats

    toPat :: Pat -> H.Pat Loc
    toPat (PVar loc var) = H.PVar loc (toIdentName var)

    toRhs :: Alt Lang -> H.Rhs Loc
    toRhs Alt{..} = H.UnGuardedRhs (HM.getLoc alt'expr) (toHaskExp alt'expr)


toIdentName :: VarName -> H.Name Loc
toIdentName (VarName loc name) = H.Ident loc (T.unpack name)

toSymbolName :: VarName -> H.Name Loc
toSymbolName VarName{..} = H.Symbol varName'loc (T.unpack varName'name)


toType :: Signature -> H.Type Loc
toType x = case splitToPreds x of
  (_, ty) -> singleType ty
  where
    splitToPreds = cata go . HM.unSignature
      where
        go = \case
          HM.MonoT ty                  -> ([], ty)
          HM.ForAllT loc name (xs, ty) -> (VarName loc name : xs, ty)

    singleType :: HM.Type Loc -> H.Type Loc
    singleType = cata go . HM.unType
      where
        go = \case
          HM.VarT loc var   -> H.TyVar loc (toIdentName $ VarName loc var)
          HM.ConT loc con   -> H.TyCon loc (toQName $ VarName loc con)
          HM.ArrowT loc a b -> H.TyFun loc a b
          -- TODO: How to express typles?
          -- TTuple loc as -> H.TyTuple loc H.Boxed (fmap singleType as)
          HM.AppT loc a b   -> H.TyApp loc a b


toQName :: VarName -> H.QName Loc
toQName x = H.UnQual (HM.getLoc x) $ toIdentName x

toSymbolQName :: VarName -> H.QName Loc
toSymbolQName x@(VarName loc _) = H.UnQual loc $ toSymbolName x

toVar :: Loc -> VarName -> H.Exp Loc
toVar loc name = H.Var loc (toQName name)


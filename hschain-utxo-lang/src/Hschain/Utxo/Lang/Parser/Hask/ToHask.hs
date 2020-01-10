module Hschain.Utxo.Lang.Parser.Hask.ToHask(
    toHaskExp
  , toHaskModule
) where

import Control.Monad

import Data.Fix

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Language.Haskell.Exts.Pretty

import Type.Loc
import Type.Type

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V

import qualified Language.Haskell.Exts.SrcLoc as H
import qualified Language.Haskell.Exts.Syntax as H
import qualified Language.Haskell.Exts.Parser as H


toHaskExp :: Lang -> H.Exp Loc
toHaskExp (Fix expr) = case expr of
  Var loc name -> toVar loc name
  Apply loc a b -> H.App loc (rec a) (rec b)
  InfixApply loc a v b -> H.InfixApp loc (rec a) (H.QVarOp (getLoc v) $ toSymbolQName' v) (rec b)
  Lam loc name a -> H.Lambda loc [H.PVar (getLoc name) $ toIdentName' name] (rec a)
  LamList loc vs a -> H.Lambda loc (fmap (\v -> H.PVar (getLoc v) $ toIdentName' v) vs) (rec a)
  Let loc bg a -> undefined
  LetRec loc name a b -> undefined
  Ascr loc a ty -> H.ExpTypeSig loc (rec a) (toType (Forall loc [] (Qual loc [] ty)))
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
    ap f x = H.App (getLoc f) (toVar (getLoc f) f) (rec x)
    ap2 f x y = H.App (getLoc y) (H.App (getLoc f) (toVar (getLoc f) f) (rec x)) (rec y)
    op2 f x y = H.InfixApp (getLoc f) (rec x) (H.QVarOp (getLoc f) $ toQName' f) (rec y)


    fromUnOp loc op a = case op of
      Not       -> ap (VarName loc "not") a
      Neg       -> ap (VarName loc "negate") a
      TupleAt n -> ap2 (VarName loc "tupleAt") (Fix $ PrimE loc $ PrimInt loc n) a

    fromBimOp loc op = case op of
      And                   -> op2 "&&"
      Or                    -> op2 "||"
      Plus                  -> op2 "+"
      Minus                 -> op2 "-"
      Times                 -> op2 "*"
      Div                   -> op2 "/"
      Equals                -> op2 "=="
      NotEquals             -> op2 "/="
      LessThan              -> op2 "<"
      GreaterThan           -> op2 ">"
      LessThanEquals        -> op2 "<="
      GreaterThanEquals     -> op2 ">="
      ComposeFun            -> op2 "."
      where
        op2 name a b = H.InfixApp loc (rec a) (H.QVarOp loc $ H.UnQual loc $ H.Ident loc name) (rec b)

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
      VecAppend loc a b -> op2 (VarName loc "++") a b
      VecAt loc a b     -> op2 (VarName loc "!") a b
      VecLength loc     -> toVar loc (VarName loc "length")
      VecMap loc        -> toVar loc (VarName loc "map")
      VecFold loc       -> toVar loc (VarName loc "fold")

    fromText _ = \case
      TextAppend loc a b  -> op2 (VarName loc "<>") a b
      ConvertToText loc   -> toVar loc (VarName loc "show")
      TextLength loc      -> toVar loc (VarName loc "lengthText")
      TextHash loc algo   -> case algo of
        Sha256     -> toVar loc (VarName loc "sha256")
        Blake2b256 -> toVar loc (VarName loc "blake2b256")

    fromBox :: Loc -> BoxExpr Lang -> H.Exp Loc
    fromBox _ = \case
      PrimBox loc box     -> fromPrimBox loc box
      BoxAt loc a field   -> fromBoxField loc a field

    fromPrimBox loc Box{..} = H.RecConstr loc (qname "Box")
      [ field "box'id"     $ prim $ PrimString loc $ unBoxId box'id
      , field "box'value"  $ prim $ PrimMoney  loc $ box'value
      , field "box'script" $ prim $ PrimString loc $ unScript box'script
      , field "box'args"   $ Fix $ VecE loc $ NewVec loc (V.fromList $ fmap (\(a, b) -> Fix $ Tuple loc (V.fromList [a, b])) args)
      ]
      where
        qname a = toQName' $ VarName loc a
        field name a = H.FieldUpdate loc (qname name) (rec a)
        prim = Fix . PrimE loc
        args = fmap (\(txt, val) -> (Fix $ PrimE loc $ PrimString loc txt, Fix $ PrimE loc val)) $ M.toList box'args

    fromBoxField loc a = \case
      BoxFieldId          -> get "getBoxId"
      BoxFieldValue       -> get "getValue"
      BoxFieldScript      -> get "getScript"
      BoxFieldArg b       -> ap2 (VarName loc "getBoxArg") b a
      where
        get name = ap (VarName loc name) a

toLiteral :: Loc -> Prim -> H.Exp Loc
toLiteral mainLoc = \case
  PrimInt loc    x -> lit $ H.Int loc (fromIntegral x) (show x)
  PrimMoney loc  x -> lit $ H.Frac loc (realToFrac x) (show x)
  PrimDouble loc x -> lit $ H.Frac loc (realToFrac x) (show x)
  PrimString loc x -> lit $ H.String loc (T.unpack x) (T.unpack x)
  PrimBool loc   x -> H.Con mainLoc $ bool loc x
  PrimSigma loc  x -> sigma loc x
  where
    lit = H.Lit mainLoc
    bool loc x = H.UnQual loc $ H.Ident loc $ show x

    sigma :: Loc -> Sigma PublicKey -> H.Exp Loc
    sigma loc x = cata go x
      where
        go :: SigmaExpr PublicKey (H.Exp Loc) -> H.Exp Loc
        go = \case
          SigmaPk pkey -> let keyTxt = publicKeyToText pkey
                            in  ap (VarName loc "pk") $ lit $ H.String loc (T.unpack keyTxt) (T.unpack keyTxt)
          SigmaAnd a b -> op2 "&&" a b
          SigmaOr  a b -> op2 "||" a b

        op2 :: String -> H.Exp Loc -> H.Exp Loc -> H.Exp Loc
        op2 name a b = H.InfixApp loc a (H.QVarOp loc $ H.UnQual loc $ H.Ident loc name) b

        ap f x = H.App (getLoc f) (toVar (getLoc f) f) x

toHaskModule :: Module -> H.Module Loc
toHaskModule (Module loc bs) = H.Module loc Nothing [] [] (toDecl =<< bs)
  where
    toDecl :: BindGroup Lang -> [H.Decl Loc]
    toDecl BindGroup{..} = concat
      [ toDeclExpl =<< bindGroup'expl
      , concat $ fmap (toDeclImpl =<< ) bindGroup'impl
      ]

    toDeclImpl :: Impl Lang -> [H.Decl Loc]
    toDeclImpl Impl{..} = return $ H.FunBind (getLoc impl'name) $ fmap (toMatch impl'name) impl'alts

    toDeclExpl :: Expl Lang -> [H.Decl Loc]
    toDeclExpl Expl{..} =
      [ signature
      , funBind ]
      where
        signature = H.TypeSig tyLoc [H.Ident tyLoc (T.unpack $ id'name expl'name)] (toType expl'type)
        funBind = H.FunBind (getLoc expl'name) $ fmap (toMatch expl'name) expl'alts

        tyLoc = getLoc expl'type


    toMatch :: Id -> Alt Lang -> H.Match Loc
    toMatch name alt = H.Match (getLoc name) (toIdentName name) (toPats alt) (toRhs alt) Nothing

    toPats :: Alt a -> [H.Pat Loc]
    toPats = fmap toPat . alt'pats

    toPat :: Pat -> H.Pat Loc
    toPat (PVar loc var) = H.PVar loc (toIdentName var)

    toRhs :: Alt Lang -> H.Rhs Loc
    toRhs Alt{..} = H.UnGuardedRhs (getLoc alt'expr) (toHaskExp alt'expr)

toIdentName :: Id -> H.Name Loc
toIdentName Id{..} = H.Ident id'loc (T.unpack id'name)

toIdentName' :: VarName -> H.Name Loc
toIdentName' (VarName loc name) = H.Ident loc (T.unpack name)

toSymbolName :: Id -> H.Name Loc
toSymbolName Id{..} = H.Symbol id'loc (T.unpack id'name)

toSymbolName' :: VarName -> H.Name Loc
toSymbolName' (VarName loc name) = H.Symbol loc (T.unpack name)


toType :: Scheme -> H.Type Loc
toType (Forall loc _ (Qual _ ps ty)) = case ps of
  [] -> noPreds ty
  _  -> withPreds ps ty
  where
    noPreds = singleType
    withPreds ps ty = H.TyForall loc Nothing (Just $ toContext ps) (singleType ty)

    singleType = \case
      TVar loc var  -> H.TyVar loc (toIdentName $ (\(Tyvar _ v _) -> v) var)
      TCon loc con  -> H.TyCon loc (toQName $ (\(Tycon _ idx _) -> idx) con)
      TFun loc a b  -> H.TyFun loc (singleType a) (singleType b)
      TTuple loc as -> H.TyTuple loc H.Boxed (fmap singleType as)
      TAp loc a b   -> H.TyApp loc (singleType a) (singleType b)
      TGen loc n    -> H.TyCon loc (H.UnQual loc $ H.Ident loc $ mappend "Gen-" $ show n)

    toContext ps = case ps of
      [p] -> H.CxSingle (getLoc p) $ toAsst p
      p:_ -> H.CxTuple (getLoc p) $ fmap toAsst ps
      []  -> H.CxEmpty loc

    toAsst (IsIn loc1 cls ty) = H.ClassA loc1 (toQName cls) [singleType ty]


toQName :: Id -> H.QName Loc
toQName x = H.UnQual (getLoc x) $ toIdentName x

toQName' :: VarName -> H.QName Loc
toQName' x@(VarName loc _) = H.UnQual loc $ toIdentName' x

toSymbolQName :: Id -> H.QName Loc
toSymbolQName x = H.UnQual (getLoc x) $ toSymbolName x

toSymbolQName' :: VarName -> H.QName Loc
toSymbolQName' x@(VarName loc _) = H.UnQual loc $ toSymbolName' x

toVar :: Loc -> VarName -> H.Exp Loc
toVar loc name = H.Var loc (toQName $ fromVarName name)


{-# OPTIONS_GHC -Wno-orphans #-}
-- | Type inference for core programms
module Hschain.Utxo.Lang.Compile.Infer(
  annotateTypes
) where

import Hex.Common.Text

import Data.Function
import Data.Fix
import Data.Foldable
import Data.String
import qualified Data.Map.Strict as M

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Dependencies
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), TypeCore(..))
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (primToType,primopToType,runCheck)
import Hschain.Utxo.Lang.Expr ( Loc, noLoc, VarName(..), typeCoreToType, varT, funT, listT, tupleT
                              , arrowT, intT, boolT, bytesT, textT, sigmaT, boxT, argsT)
import Hschain.Utxo.Lang.Core.Compile.Expr      (monoPrimopNameMap)

import qualified Language.HM as H
import qualified Data.Sequence as S


-- | We need this type for type-inference algorithm
data Tag
  = VarTag !Name    -- ^ simple variables
  | IfTag           -- ^ if-expressions
  | ConstrTag !Int  -- ^ integer tags for constructors
  deriving (Show, Eq, Ord)

instance H.IsVar Tag where
  intToVar n = VarTag $ mappend "$$" (showt n)
  prettyLetters = fmap (VarTag . fromString) $ [1..] >>= flip replicateM ['a'..'z']

-- todo: ugly hack, something wrong with this function
-- do we really need it
fromTag :: Tag -> Name
fromTag = \case
  VarTag name -> name
  IfTag       -> "if"
  ConstrTag n -> showt n

instance IsString Tag where
  fromString = VarTag . fromString

instance H.IsPrim PrimLoc where
  type PrimLoc PrimLoc = Loc
  type PrimVar PrimLoc = Tag

  getPrimType (PrimLoc loc p) = H.setLoc loc $ typeCoreToType @() $ primToType p

eraseLoc :: H.Type loc Name -> H.Type Loc Tag
eraseLoc = H.mapLoc (const noLoc) . fmap VarTag

eraseWith :: Loc -> H.Type loc Name -> H.Type Loc Tag
eraseWith loc = H.mapLoc (const loc) . fmap VarTag

toType :: H.Type Loc Tag -> H.Type () Name
toType = H.mapLoc (const ()) . fmap fromTag

-- | Infers types for all subexpressions
annotateTypes :: forall m . MonadLang m => LamProg -> m TypedLamProg
annotateTypes =
  fmap (AnnLamProg . reverse . snd) . foldM go (libTypeContext, []) . unLamProg . orderDependencies
  where
    go (ctx, prog) comb = do
      (combT, combTyped) <- typeDef ctx comb
      return (H.insertContext (VarTag $ varName'name $ def'name comb) combT ctx, combTyped : prog)

    typeDef :: H.Context Loc Tag -> Comb Name -> m (H.Type Loc Tag, TypedDef)
    typeDef ctx comb = do
      (combT, term) <- liftEither $ either fromErr Right $ H.inferTerm ctx (toInferExpr $ getCombExpr comb)
      body <- fromInferExpr term
      let (bodyExpr, args) = collectArgs S.empty body
      return $ (combT, comb
        { def'args = args
        , def'body = bodyExpr
        })

    collectArgs res (Fix expr) = case expr of
      Ann _ e -> case e of
        ELam _ args a -> collectArgs (res <> S.fromList args) a
        _            -> (Fix expr, toList res)


    fromErr = Left . TypeError . fmap fromTag

    getCombExpr Def{..}
      | null def'args = def'body
      | otherwise     = Fix $ ELam (H.getLoc def'name) def'args def'body
                         -- todo consider to add locations to definitions

    toInferExpr :: ExprLam Name -> H.Term PrimLoc Loc Tag
    toInferExpr = cata $ \case
      EVar loc name   -> H.varE loc (VarTag name)
      EPrim loc prim  -> H.primE loc prim
      EPrimOp{}       -> error "No primop are accessible before type checking"
      EAp loc a b     -> H.appE loc a b
      ELam loc args e -> foldr (H.lamE loc) e (fmap VarTag args)
      EIf loc a b c   -> H.appE loc (H.appE loc (H.appE loc (H.varE loc IfTag) a) b) c
      EBottom loc     -> H.bottomE loc
      EConstr loc ty tag arity -> H.constrE loc (eraseWith loc ty) (ConstrTag tag) arity
      ELet loc bs e   -> foldr (\b rhs -> H.letE loc [fromBind loc b] rhs) e bs
      EAssertType loc e ty -> H.assertTypeE loc e (eraseWith loc ty)
      ECase loc e alts -> H.caseE loc e (fmap fromAlt alts)

    -- todo: we do need to use VarName to keep info on bind locations
    --  for now we write wrong locations...
    fromBind loc (bind, e) = H.Bind
      { H.bind'loc = loc
      , H.bind'lhs = VarTag bind
      , H.bind'rhs = e
      }

    fromAlt CaseAlt{..} = H.CaseAlt
      { H.caseAlt'loc        = caseAlt'loc
      , H.caseAlt'tag        = ConstrTag caseAlt'tag
      , H.caseAlt'args       = fmap toArg caseAlt'args
      , H.caseAlt'constrType = eraseWith caseAlt'loc $ caseAlt'constrType
      , H.caseAlt'rhs        = caseAlt'rhs
      }
      where
        -- we need to know the types of the constructors on this stage:
        toArg (Typed val ty) = H.Typed (eraseWith caseAlt'loc ty) (caseAlt'loc, VarTag val)

    fromInferExpr :: H.TyTerm PrimLoc Loc Tag -> m TypedExprLam
    fromInferExpr (H.TyTerm x) = flip cataM x $ \case
      H.Ann ty expr -> fmap (Fix . Ann (toType ty)) $ case expr of
        H.Var loc name -> pure $ EVar loc (fromTag name)
        H.Prim loc p   -> pure $ EPrim loc p
        H.App _ (Fix (Ann _ (EAp _ (Fix (Ann _ (EAp _ (Fix (Ann _ (EVar vloc "if"))) c))) t))) e -> pure $ EIf vloc c t e
        H.App loc a b -> pure $ EAp loc a b
        H.Lam loc arg e -> fmap (\t -> ELam loc [Typed (fromTag arg) (toType t)] e) $ getLamArgType ty
        H.Bottom loc -> pure $ EBottom loc
        H.AssertType _ (Fix (Ann _ a)) _ -> pure $ a
        H.Let loc bs e -> toLet loc bs e
        H.LetRec loc bs e -> toLet loc bs e
        H.Case loc e alts -> fmap (ECase loc e) (mapM toAlt alts)
        H.Constr loc conTy tag n ->
          case tag of
            ConstrTag m -> pure $ EConstr loc (toType conTy) m n
            _           -> throwError $ InternalError $ NonIntegerConstrTag (fromTag tag)

    toAlt alt =
      case H.caseAlt'tag alt of
        ConstrTag tagId -> pure $
          CaseAlt
            { caseAlt'loc        = H.caseAlt'loc alt
            , caseAlt'tag        = tagId
            , caseAlt'args       = fmap (\t -> Typed (fromTag $ snd $ H.typed'value t) (toType $ H.typed'type t)) $ H.caseAlt'args alt
            , caseAlt'constrType = toType $ H.caseAlt'constrType alt
            , caseAlt'rhs        = H.caseAlt'rhs alt
            }
        other -> throwError $ InternalError $ NonIntegerConstrTag (fromTag other)


    toLet loc binds body = pure $ ELet loc (fmap toBind binds) body

    toBind b = (Typed (fromTag $ H.bind'lhs b) (getType rhs), rhs)
      where
        rhs = H.bind'rhs b
        getType (Fix (Ann t _)) = t

    getLamArgType (H.Type (Fix x)) = case x of
      H.ArrowT _ a _ -> pure $ H.Type a
      _              -> throwError $ InternalError $ NonLamType


libTypeContext :: H.Context Loc Tag
libTypeContext = (H.Context $ M.fromList
  [ (IfTag, forA $ funT [boolT, aT, aT] aT)
  , (VarTag "pk", H.monoT $ funT [textT] sigmaT)
  , (VarTag "listAt", H.forAllT noLoc "a" $ H.monoT $ funT [listT (varT "a"), intT] (varT "a"))
  , (VarTag "length", H.forAllT noLoc "a" $ H.monoT $ funT [listT (varT "a")] intT)
  ])
  <> genericCompareOps
  <> fromPrimOps
  where
    aT = varT "a"
    forA = H.forAllT noLoc (VarTag "a") . H.monoT

    genericCompareOps = H.Context $ M.fromList $ fmap (, cmpT) $
      [ "==", "/=", "<", ">", "<=", ">=" ]

    cmpT = forA $ aT `arrowT` (aT `arrowT` boolT)

    fromPrimOps = H.Context $ M.fromList
      [ (VarTag nm, H.monoT $ typeCoreToType ty)
      | (nm,op) <- M.toList monoPrimopNameMap
      , let Right ty = runCheck mempty $ primopToType op
      ]

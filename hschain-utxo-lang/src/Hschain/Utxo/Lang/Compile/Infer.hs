{-# OPTIONS_GHC -Wno-orphans #-}
-- | Type inference for core programms
module Hschain.Utxo.Lang.Compile.Infer(
  annotateTypes
) where

import Hex.Common.Text

import Data.Fix
import Data.Foldable
import Data.String
import qualified Data.Map.Strict as M

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Dependencies
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Types             (Name, Typed(..))
import Hschain.Utxo.Lang.Core.Compile.Expr (PrimCon(..), conType)
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (primToType,primopToType,runCheck)
import Hschain.Utxo.Lang.Expr ( Loc, noLoc, VarName(..), typeCoreToType, varT, funT, listT
                              , arrowT, intT, boolT, bytesT, sigmaT
                              , monoPrimopNameMap)

import qualified Type.Check.HM as H
import qualified Data.Sequence as S
import qualified Data.Vector   as V

import Hschain.Utxo.Lang.Lib.Base (baseLibTypeContext)

data HschainLang

instance H.Lang HschainLang where
  type Src  HschainLang = Loc
  type Var  HschainLang = Tag
  type Prim HschainLang = PrimLoc

  getPrimType (PrimLoc loc p) = eraseWith loc $ typeCoreToType $ primToType p

-- | We need this type for type-inference algorithm
data Tag
  = VarTag !Name                           -- ^ simple variables
  | IfTag                                  -- ^ if-expressions
  | ConstrTag !(PrimCon (H.Type () Name))  -- ^ integer tags for constructors
  deriving (Show, Eq, Ord)

instance H.IsVar Tag where
  prettyLetters = fmap VarTag H.prettyLetters

-- todo: ugly hack, something wrong with this function
-- do we really need it
fromTag :: Tag -> Name
fromTag = \case
  VarTag name -> name
  IfTag       -> "if"
  ConstrTag n -> showt n

instance IsString Tag where
  fromString = VarTag . fromString

eraseWith :: Loc -> H.Type () Name -> H.Type Loc Tag
eraseWith loc = H.setLoc loc . fmap VarTag

toType :: H.Type Loc Tag -> H.Type () Name
toType = H.setLoc () . fmap fromTag

-- | Infers types for all subexpressions
annotateTypes :: forall m . MonadLang m => LamProg -> m TypedLamProg
annotateTypes =
  fmap (AnnLamProg . reverse . snd) . foldM go (libTypeContext, []) . unLamProg . orderDependencies
  where
    go (ctx, prog) comb = do
      (combT, combTyped) <- typeDef ctx comb
      return (H.insertCtx (VarTag $ varName'name $ def'name comb) (H.monoT combT) ctx, combTyped : prog)

    typeDef :: H.Context Loc Tag -> Comb Name -> m (H.Type Loc Tag, TypedDef)
    typeDef ctx comb = do
      infExpr <- toInferExpr $ getCombExpr comb
      term <- liftEither $ either fromErr Right $ H.inferTerm ctx infExpr
      body <- fromInferExpr term
      let (bodyExpr, args) = collectArgs S.empty body
      return $ (H.termType term, comb
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

    toInferExpr :: ExprLam Name -> m (H.Term PrimLoc Loc Tag)
    toInferExpr = cataM $ \case
      EVar loc name        -> pure $ H.varE loc (VarTag name)
      EPrim loc prim       -> pure $ H.primE loc prim
      EPrimOp{}            -> unexpected "No primop are accessible before type checking"
      EAp loc a b          -> pure $ H.appE loc a b
      ELam loc args e      -> pure $ foldr (H.lamE loc) e (fmap VarTag args)
      EIf loc a b c        -> pure $ H.appE loc (H.appE loc (H.appE loc (H.varE loc IfTag) a) b) c
      EBottom loc          -> pure $ H.bottomE loc
      EConstr loc tag      -> maybe failedToConverType (\ty -> pure $ H.constrE loc (fmap VarTag $ H.setLoc loc ty) (ConstrTag tag)) $ conType tag
      ELet loc bs e        -> pure $ foldr (\b rhs -> H.letE loc (fromBind loc b) rhs) e bs
      EAssertType loc e ty -> pure $ H.assertTypeE loc e (eraseWith loc ty)
      ECase loc e alts     -> fmap (H.caseE loc e) (mapM fromAlt alts)

    -- todo: we do need to use VarName to keep info on bind locations
    --  for now we write wrong locations...
    fromBind loc (bind, e) = H.Bind
      { H.bind'loc = loc
      , H.bind'lhs = VarTag bind
      , H.bind'rhs = e
      }

    failedToConverType = unexpected "Failed to convert type"

    fromAlt CaseAlt{..} = do
      (argTypes, constrType) <- maybe failedToConverType (pure . H.extractFunType) $ conType caseAlt'tag
      return $ H.CaseAlt
        { H.caseAlt'loc        = caseAlt'loc
        , H.caseAlt'tag        = ConstrTag caseAlt'tag
        , H.caseAlt'args       = zipWith H.Typed (fmap (eraseWith caseAlt'loc) argTypes) $ fmap (\val -> (caseAlt'loc, VarTag val)) caseAlt'args
        , H.caseAlt'constrType = fmap VarTag $ H.setLoc caseAlt'loc constrType
        , H.caseAlt'rhs        = caseAlt'rhs
        }

    fromInferExpr :: H.TyTerm PrimLoc Loc Tag -> m TypedExprLam
    fromInferExpr (H.TyTerm x) = flip cataM x $ \case
      H.Ann tyTag expr ->
        let ty = toType tyTag
        in  fmap (Fix . Ann ty) $ case expr of
              H.Var loc name -> pure $ EVar loc (fromTag name)
              H.Prim loc p   -> pure $ EPrim loc p
              H.App _ (Fix (Ann _ (EAp _ (Fix (Ann _ (EAp _ (Fix (Ann _ (EVar vloc "if"))) c))) t))) e -> pure $ EIf vloc c t e
              H.App loc a b -> pure $ EAp loc a b
              H.Lam loc arg e -> fmap (\t -> ELam loc [Typed (fromTag arg) t] e) $ getLamArgType ty
              H.Bottom loc -> pure $ EBottom loc
              H.AssertType _ (Fix (Ann _ a)) _ -> pure $ a
              H.Let loc bs e -> toLet loc bs e
              H.LetRec loc bs e -> toLetRec loc bs e
              H.Case loc e alts -> fmap (ECase loc e) (mapM toAlt alts)
              H.Constr loc conTy tag ->
                case tag of
                  ConstrTag m -> fmap (EConstr loc) $ specifyConstr (toType conTy) m
                  _           -> throwError $ InternalError $ NonIntegerConstrTag (fromTag tag)

    toAlt alt =
      case H.caseAlt'tag alt of
        ConstrTag polyCon -> do
          monoCon <- specifyCaseAlt ty polyCon
          pure $ CaseAlt
            { caseAlt'loc        = H.caseAlt'loc alt
            , caseAlt'tag        = monoCon
            , caseAlt'args       = fmap (\t -> fromTag $ snd $ H.typed'value t) $ H.caseAlt'args alt
            , caseAlt'rhs        = H.caseAlt'rhs alt
            }
        other -> throwError $ InternalError $ NonIntegerConstrTag (fromTag other)
      where
        ty = toType $ H.caseAlt'constrType alt

    specifyConstr :: H.Type () Name -> PrimCon (H.Type () Name) -> m (PrimCon (H.Type () Name))
    specifyConstr ty = specifyCaseAlt (snd $ H.extractFunType ty)

    -- | We substitute polymorphic vars inside PrimCons
    -- with concrete monomorphic types that were derived.
    specifyCaseAlt :: H.Type () Name -> PrimCon (H.Type () Name) -> m (PrimCon (H.Type () Name))
    specifyCaseAlt (H.Type (Fix ty)) = \case
      ConNil  _    -> fromList ConNil
      ConCons _    -> fromList ConCons
      ConNothing _ -> fromMaybe ConNothing
      ConJust _    -> fromMaybe ConJust
      ConUnit      -> pure ConUnit
      ConTuple _   -> case ty of
                        H.TupleT _ ts -> pure $ ConTuple $ fmap H.Type $ V.fromList ts
                        other         -> unexpected $ "Expected tuple, got: " <> showt other
      ConSum n _   -> case ty of
                        H.ConT _ _ ts -> pure $ ConSum n $ fmap H.Type $ V.fromList ts
                        other         -> unexpected $ "Expected sum-type, got: " <> showt other
      where
        fromList con = case ty of
            H.ListT _ a -> pure $ con $ H.Type a
            other       -> unexpected $ "Expected list, got: " <> showt other

        fromMaybe con = case ty of
                    H.ConT _ "Maybe" [a] -> pure $ con $ H.Type a
                    other                -> unexpected $ "Expected maybe, got: " <> showt other

    toLet loc bind body = pure $ ELet loc [toBind bind] body
    toLetRec loc binds body = pure $ ELet loc (fmap toBind binds) body

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
  , (VarTag "pk", H.monoT $ funT [bytesT] sigmaT)
  , (VarTag "listAt", H.forAllT noLoc "a" $ H.monoT $ funT [listT (varT "a"), intT] (varT "a"))
  , (VarTag "length", H.forAllT noLoc "a" $ H.monoT $ funT [listT (varT "a")] intT)
  ])
  <> genericCompareOps
  <> fromPrimOps
  <> toTagContext baseLibTypeContext
  where
    aT = varT "a"
    forA = H.forAllT noLoc (VarTag "a") . H.monoT

    genericCompareOps = H.Context $ M.fromList $ fmap (, cmpT) $
      [ "==", "/=", "<", ">", "<=", ">=" ]

    cmpT = forA $ aT `arrowT` (aT `arrowT` boolT)

    fromPrimOps = H.Context $ M.fromList
      [ (VarTag nm, H.monoT $ typeCoreToType ty)
      | (nm,op) <- M.toList monoPrimopNameMap
      , let Right ty = runCheck $ primopToType op
      ]

    toTagContext (H.Context m) = H.Context $ M.map (fmap VarTag) $ M.mapKeys VarTag m


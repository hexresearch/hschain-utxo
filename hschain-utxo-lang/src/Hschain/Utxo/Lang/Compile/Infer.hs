{-# OPTIONS_GHC -Wno-orphans #-}
-- | Type inference for core programms
module Hschain.Utxo.Lang.Compile.Infer(
    TypedProg
  , annotateTypes
) where

import Hex.Common.Text

import Data.Fix
import Data.String

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Dependencies
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type, Prim(..))
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (primToType)
import Hschain.Utxo.Lang.Expr (Loc, noLoc)

import qualified Language.HM as H

type TypedProg = AnnProg Type (Typed Name)
type Context = H.Context Loc Tag

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


instance H.IsPrim Prim where
  type PrimLoc Prim = Loc
  type PrimVar Prim = Tag

  getPrimType = H.mapLoc (const noLoc) . fmap VarTag . primToType

eraseLoc :: Type -> H.Type Loc Tag
eraseLoc = H.mapLoc (const noLoc) . fmap VarTag

toType :: H.Type Loc Tag -> Type
toType = H.mapLoc (const ()) . fmap fromTag

-- | Infers types for all subexpressions
annotateTypes :: forall m . MonadLang m => CoreProg -> m TypedProg
annotateTypes =
  fmap (reverse . snd) . foldM go (mempty, []) . unCoreProg . orderDependencies
  where
    go (ctx, prog) comb = do
      (combT, combTyped) <- typeDef ctx comb
      return (H.insertContext (VarTag $ def'name comb) combT ctx, combTyped : prog)

    typeDef :: Context -> Comb Name -> m (H.Type Loc Tag, AnnComb Type (Typed Name))
    typeDef ctx comb = do
      (combT, term) <- liftEither $ either fromErr Right $ H.inferTerm ctx (toInferExpr $ getCombExpr comb)
      body <- fromInferExpr term
      return $ (combT, comb
        { def'args = []
        , def'body = body
        })

    fromErr = Left . TypeError . fmap fromTag

    getCombExpr Def{..}
      | null def'args = def'body
      | otherwise     = Fix $ ELam noLoc def'args def'body
                         -- todo consider to add locations to definitions }{

    toInferExpr :: Expr Name -> H.Term Prim Loc Tag
    toInferExpr = cata $ \case
      EVar loc name   -> H.varE loc (VarTag name)
      EPrim loc prim  -> H.primE loc prim
      EAp loc a b     -> H.appE loc a b
      ELam loc args e -> foldr (H.lamE loc) e (fmap VarTag args)
      EIf loc a b c   -> H.appE loc (H.appE loc (H.appE loc (H.varE loc IfTag) a) b) c
      EBottom loc     -> H.bottomE loc
      EConstr loc ty tag arity -> H.constrE loc (H.mapLoc (const noLoc) $ fmap VarTag ty) (ConstrTag tag) arity
      ELet loc bs e   -> H.letE loc (fmap (fromBind loc) bs) e
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
      , H.caseAlt'constrType = eraseLoc caseAlt'constrType
      , H.caseAlt'rhs        = caseAlt'rhs
      }
      where
        -- we need to know the types of the constructors on this stage:
        toArg (Typed val ty) = H.Typed (eraseLoc ty) (VarTag val)

    fromInferExpr :: H.TyTerm Prim Loc Tag -> m (AnnExpr Type (Typed Name))
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
            , caseAlt'args       = fmap (\t -> Typed (fromTag $ H.typed'value t) (toType $ H.typed'type t)) $ H.caseAlt'args alt
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

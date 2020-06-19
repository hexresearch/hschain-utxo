{-# OPTIONS_GHC -Wno-orphans #-}
-- | Type inference for core programms
module Hschain.Utxo.Lang.Compile.Infer(
    TypedProg
  , annotateTypes
  , makeMonomorphic
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

-- | Infers types for all subexpressions
annotateTypes :: forall m . MonadLang m => CoreProg -> m TypedProg
annotateTypes = fmap (reverse . snd) . foldM go (mempty, []) . orderDependencies
  where
    go (ctx, prog) comb = do
      (combT, combTyped) <- typeDef ctx comb
      return (H.insertContext (VarTag $ def'name comb) combT ctx, combTyped : prog)

    typeDef :: Context -> Comb Name -> m (H.Type Loc Tag, AnnComb Type (Typed Name))
    typeDef ctx comb = do
      (combT, term) <- liftEither $ either fromErr Right $ H.inferTerm ctx (toInferExpr $ getCombExpr comb)
      return $ (combT, comb
        { def'args = []
        , def'body = fromInferExpr term
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
{-
-- | Case alternatives
data CaseAlt loc v a = CaseAlt
  { caseAlt'loc   :: loc
  -- ^ source code location
  , caseAlt'tag   :: v
  -- ^ tag of the constructor
  , caseAlt'args  :: [Typed loc v v]
  -- ^ arguments of the pattern matching
  , caseAlt'constrType :: Type loc v
  -- ^ type of the matched expression, they should be the same for all cases
  , caseAlt'rhs   :: a
  -- ^ right-hand side of the case-alternative
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)
-}
    fromAlt CaseAlt{..} = H.CaseAlt
      { H.caseAlt'loc  = caseAlt'loc
      , H.caseAlt'tag  = ConstrTag caseAlt'tag
      , H.caseAlt'args = fmap toArg caseAlt'args
      , H.caseAlt'constrType = ty
      , H.caseAlt'rhs = caseAlt'rhs
      }
      where
        -- we need to know the types of the constructors on this stage:
        ty = undefined
        toArg = undefined
{-
data CaseAlt bind a = CaseAlt
  { caseAlt'loc   :: !Loc
  , caseAlt'tag   :: !Int
  -- ^ integer tag of the constructor
  -- (integer substitution for the name of constructor)
  , caseAlt'args  :: [bind]
  -- ^ arguments of the pattern matching
  , caseAlt'rhs   :: a
  -- ^ right-hand side of the case-alternative
  }
  deriving (Show, Eq, Functor, Foldable, Traversable)
-}
    fromInferExpr = undefined


-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedProg -> m TypedProg
makeMonomorphic = undefined


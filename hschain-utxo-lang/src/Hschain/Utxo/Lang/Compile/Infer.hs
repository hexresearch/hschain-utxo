-- | Type inference for core programms
module Hschain.Utxo.Lang.Compile.Infer(
    TypedProg
  , annotateTypes
  , makeMonomorphic
) where

import Data.Fix

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Dependencies
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type)
import Hschain.Utxo.Lang.Expr (Loc, noLoc)

import qualified Language.HM as H

type TypedProg = AnnProg Type (Typed Name)
type Context = H.Context Loc Name

-- | Infers types for all subexpressions
annotateTypes :: forall m . MonadLang m => CoreProg -> m TypedProg
annotateTypes = fmap (reverse . snd) . foldM go (mempty, []) . orderDependencies
  where
    go (ctx, prog) comb = do
      (combT, combTyped) <- typeDef ctx comb
      return (H.insertContext (def'name comb) combT ctx, combTyped : prog)

    typeDef :: Context -> Comb Name -> m (H.Type Loc Name, AnnComb Type (Typed Name))
    typeDef ctx comb = do
      (combT, term) <- liftEither $ either (Left . TypeError) Right $ H.inferTerm ctx (toInferExpr $ getCombExpr comb)
      return $ (combT, comb
        { def'args = []
        , def'body = fromInferExpr term
        })

    getCombExpr Def{..}
      | null def'args = def'body
      | otherwise     = Fix $ ELam noLoc def'args def'body
                         -- todo consider to add locations to definitions }{
                         --
    toInferExpr = cata $ \case
      EVar loc name -> H.varE loc name
     {-
      -- ^ variables
      | EPrim Loc !Prim
      -- ^ constant primitive
      | EAp Loc a a
      -- ^ application
      | ELet Loc [(bind, a)] a
      -- ^ lent bindings
      | ELam Loc [bind] a
      -- ^ lambda abstraction
      | EIf Loc a a a
      -- ^ if expressions
      | ECase Loc !a [CaseAlt bind a]
      -- ^ case alternatives
      | EConstr Loc Type !Int !Int
      -- ^ constructor with tag and arity, also we should provide the type
      -- of constructor as afunction for a type-checker
      | EBottom Loc
 -}

    fromInferExpr = undefined


-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedProg -> m TypedProg
makeMonomorphic = undefined


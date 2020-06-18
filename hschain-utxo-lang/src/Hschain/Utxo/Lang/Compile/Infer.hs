-- | Type inference for core programms
module Hschain.Utxo.Lang.Compile.Infer(
    TypedProg
  , annotateTypes
  , makeMonomorphic
) where


import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Dependencies
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type)
import Hschain.Utxo.Lang.Expr (Loc)

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

    getCombExpr = undefined
    toInferExpr = undefined
    fromInferExpr = undefined


-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedProg -> m TypedProg
makeMonomorphic = undefined


-- | Turns polymorphic programs to monomorphic ones
module Hschain.Utxo.Lang.Compile.Monomorphize(
  makeMonomorphic
) where

import Control.Monad.State.Strict

import Data.Fix
import Data.Map.Strict (Map)
import Data.Sequence (Seq)
import Data.String

import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Compile.Dependencies
import Hschain.Utxo.Lang.Compile.Expr
import Hschain.Utxo.Lang.Compile.Infer (TypedProg)
import Hschain.Utxo.Lang.Core.Data.Prim (Name, Typed(..), Type, Prim(..))
import Hschain.Utxo.Lang.Core.Compile.TypeCheck (primToType)
import Hschain.Utxo.Lang.Expr (Loc, noLoc, boolT, VarName(..))

import qualified Language.HM as H
import qualified Data.Map.Strict as M
import qualified Data.Sequence as Seq

-- | Makes types monomorphic.
makeMonomorphic :: MonadLang m => TypedProg -> m TypedProg
makeMonomorphic prog = runMono (makeMono progMap)
  where
    progMap = M.fromList $ fmap (\x -> (def'name x, x)) prog

type Mono a = StateT MonoSt (Either Error) a
type ProgMap = Map Name TypedDef
type TypedDef = AnnComb Type (Typed Name)


runMono :: MonadLang m => Mono a -> m TypedProg
runMono m = liftEither $ fmap (M.elems . monoSt'prog) $ execStateT m initSt
  where
    initSt = MonoSt initSeed M.empty
    initSeed = Seq.singleton $ Typed "main" $ fromType boolT
    fromType = H.mapLoc (const ())

data MonoSt = MonoSt
  { monoSt'seeds :: Seq (Typed Name)  -- ^ Free variables found in the definition,
                                      --   with monomorphic types
  , monoSt'prog  :: ProgMap           -- ^ Result of the algorithm
  }

makeMono :: ProgMap -> Mono ()
makeMono progMap = do
  st <- get
  mapM_ (procSeed progMap $ monoSt'prog st) $ monoSt'seeds st

procSeed :: ProgMap -> ProgMap -> Typed Name -> Mono ()
procSeed progMap resultProgMap seed
  | isMonoT $ typed'type seed =
      case M.lookup name progMap of
        Just defn -> withCheckCache name (procDef progMap defn)
        Nothing   -> lift $ Left $ ExecError $ UnboundVariables [VarName noLoc name]
  | otherwise  = lift $ Left $ MonoError $ FailedToFindMonoType name
  where
    name = typed'value seed

    withCheckCache key act
      | M.member key resultProgMap = return ()
      | otherwise                  = act

procDef :: ProgMap -> TypedDef -> Mono ()
procDef progMap defn = do
  (newSeeds, defn') <- substDef progMap defn
  addSeeds newSeeds
  insertDef defn'

substDef :: ProgMap -> TypedDef -> Mono ([Typed Name], TypedDef)
substDef = undefined

isMonoT :: Type -> Bool
isMonoT (H.Type x) = flip cata x $ \case
  H.VarT _ _     -> False
  H.ConT _ _ as  -> and as
  H.ArrowT _ a b -> a && b
  H.ListT _ a    -> a
  H.TupleT _ as  -> and as

addSeeds :: [Typed Name] -> Mono ()
addSeeds seeds =
  modify' $ \st -> st { monoSt'seeds = monoSt'seeds st <> Seq.fromList seeds }

insertDef :: TypedDef -> Mono ()
insertDef defn =
  modify' $ \st -> st { monoSt'prog = M.insert (def'name defn) defn $ monoSt'prog st }


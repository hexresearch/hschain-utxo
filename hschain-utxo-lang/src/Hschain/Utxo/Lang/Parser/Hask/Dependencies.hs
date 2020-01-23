module Hschain.Utxo.Lang.Parser.Hask.Dependencies(
    Decl(..)
  , toBindGroup
) where

import Hex.Common.Control

import Control.Monad

import Data.Foldable

import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Type.Loc
import Type.Type

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Lib.Base
import Hschain.Utxo.Lang.Parser.Hask.Utils

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as L
import qualified Data.Vector as V

-- import Debug.Trace

type TypeMap  = Map VarName (Qual Type)
type FunMap   = Map VarName (Alt Lang)
type Deps     = Map VarName [VarName]
type FunOrder = [VarName]

data Decl
  = FunDecl Loc [(VarName, Alt Lang)]
  | TypeSig Loc [VarName] (Qual Type)

toBindGroup :: [Decl] -> ParseResult [BindGroup Lang]
toBindGroup ds = do
  typeMap  <- getTypeMap ds
  funMap   <- getFunMap ds
  funOrder <- orderDeps (getDeps funMap)
  renderToBinds funMap typeMap funOrder
{-
  where
    trOrder ord = trace (ppOrdDeps ord) ord

    trDeps deps = trace (ppDeps deps) deps

    ppOrdDeps ord = Text.unpack $ Text.unwords $ fmap varName'name ord

    ppDeps deps = Text.unpack $ Text.unlines $ fmap (\(k, vs) -> mconcat [ppVar k, " : ", ppVars vs]) $ Map.toList deps
      where
        ppVar VarName{..} = varName'name
        ppVars vs = Text.unwords $ fmap ppVar vs
-}

getTypeMap :: [Decl] -> ParseResult TypeMap
getTypeMap = foldM accumTypeMap Map.empty . concat . catMaybes . fmap getTypeSig
  where
    getTypeSig = \case
      TypeSig loc vs ty -> Just $ fmap (\v -> (v, ty)) vs
      _                 -> Nothing

    accumTypeMap m (v, ty) = case Map.lookup v m of
      Nothing -> return $ Map.insert v ty m
      Just _  -> parseFailed (varName'loc v) $ mconcat ["Type signature for variable ", Text.unpack $ varName'name v, " is defined twice"]

getFunMap :: [Decl] -> ParseResult FunMap
getFunMap = fmap Map.fromList . mapM toSingleName . catMaybes . fmap getFunDecl
  where
    getFunDecl = \case
      FunDecl loc xs -> Just (loc, xs)
      _              -> Nothing

    toSingleName (loc, xs) = case xs of
      [(v, alt)] -> return (v, alt)
      []         -> parseFailed loc "No cases are defined"
      (v, _):_   -> parseFailed (varName'loc v) $ mconcat ["Too many functional cases are defined for: ", Text.unpack $ varName'name v]

getDeps :: FunMap -> Deps
getDeps = fmap (Set.toList . (`Set.difference` baseNamesSet) . freeVars . altToExpr)
  where
    baseNamesSet = Set.fromList $ fmap (VarName noLoc) baseNames

    freeVars :: Lang -> Set VarName
    freeVars = cata $ \case
      Var _ v         -> Set.singleton v
      InfixApply _ a _ b -> a <> b
      Apply _ a b     -> a <> b
      Lam _ v a       -> Set.filter (/= v) a
      LamList _ vs a  -> a `Set.difference` (Set.fromList vs)
      Let _ bg a      -> (a `Set.difference` getBgNames bg) <> freeVarsBg bg
      LetRec _ v a b  -> a <> Set.filter (/= v) b
      Ascr _ a _      -> a
      PrimE _ _       -> Set.empty
      If _ a b c      -> mconcat [a, b, c]
      Pk _ a          -> a
      Tuple _ vs      -> fold $ V.toList vs
      UnOpE _ _ a     -> a
      BinOpE _ _ a b  -> mconcat [a, b]
      GetEnv _ env    -> fold env
      VecE _ vec      -> fold vec
      TextE _ txt     -> fold txt
      BoxE _ box      -> fold box
      Undef _         -> Set.empty
      Trace _ a b     -> mconcat [a, b]

    getBgNames :: BindGroup a -> Set VarName
    getBgNames BindGroup{..} = Set.fromList $
      fmap toVarName $ concat $ fmap expl'name bindGroup'expl : (fmap2 impl'name bindGroup'impl)

    freeVarsBg = fold


orderDeps :: Deps -> ParseResult FunOrder
orderDeps deps
  | null noDeps = if null deps
                    then return []
                    else recursiveFail
  | otherwise   = do
      next <- orderDeps (removeDeps withDeps)
      return $ mconcat [noDepKeys, next]
  where
    (noDeps, withDeps) = Map.partition null deps
    noDepKeys = Map.keys noDeps
    withDepKeys = Map.keys withDeps

    removeDeps = fmap (filter (not . flip Set.member blackList))
      where
        blackList = Set.fromList noDepKeys

    recursiveFail
      | not $ null noDef = return withDepKeys -- let var = head noDef
                           -- in  parseFailed (getLoc var) $ Text.unpack $ mconcat ["No defenition for free variables: ", Text.unwords (fmap varName'name $ deps Map.! var), " for variable ", varName'name var]
      | otherwise        = parseFailed noLoc "Recursive definitions not allowed" -- todo: find cycles here
                                                                                 --       to say wich vars are recursive
      where
        rhs = fold deps
        lhs = Map.keys deps

        noDef = Set.toList $ Set.difference (Set.fromList lhs) (Set.fromList rhs)

renderToBinds :: FunMap -> TypeMap -> FunOrder -> ParseResult [BindGroup Lang]
renderToBinds funs tys names = mapM toGroup names
  where
    toGroup name = case Map.lookup name funs of
      Nothing  -> parseFailedVar "Undefined variable" name
      Just f   -> return $ case Map.lookup name tys of
                    Nothing  -> implGroup name f
                    Just ty  -> explGroup name f $ Forall (getLoc ty) [] ty


    implGroup name f = BindGroup [] [[Impl (fromVarName name) [f]]]

    explGroup name f ty = BindGroup [Expl (fromVarName name) ty [f]] []


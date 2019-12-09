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
import Data.Text (Text)

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Type.Type

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Hask.Utils

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.List as L
import qualified Data.Vector as V


type TypeMap  = Map VarName Scheme
type FunMap   = Map VarName (Alt Lang)
type Deps     = Map VarName [VarName]
type FunOrder = [VarName]

data Decl
  = FunDecl Loc [(VarName, Alt Lang)]
  | TypeSig Loc [VarName] Scheme

toBindGroup :: [Decl] -> ParseResult [BindGroup Lang]
toBindGroup ds = do
  typeMap  <- getTypeMap ds
  funMap   <- getFunMap ds
  funOrder <- orderDeps funMap typeMap (getDeps funMap)
  renderToBinds funMap typeMap funOrder

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
getDeps = fmap (freeVars . alt'expr)
  where
    freeVars = cata $ \case
      Var _ v         -> [v]
      Apply _ a b     -> a <> b
      Lam _ v a       -> filter (/= v) a
      LamList _ vs a  -> a L.\\ vs
      Let _ bg a      -> (a L.\\ getBgNames bg) ++ freeVarsBg bg
      LetRec _ v a b  -> a ++ filter (/= v) b
      Ascr _ a _      -> a
      PrimE _ _       -> []
      If _ a b c      -> mconcat [a, b, c]
      Pk _ a          -> a
      Tuple _ vs      -> mconcat $ V.toList vs
      UnOpE _ _ a     -> a
      BinOpE _ _ a b  -> mconcat [a, b]
      GetEnv _ env    -> fold env
      VecE _ vec      -> fold vec
      TextE _ txt     -> fold txt
      BoxE _ box      -> fold box
      Undef _         -> []
      Trace _ a b     -> mconcat [a, b]

    getBgNames BindGroup{..} =
      fmap toVarName $ concat $ fmap expl'name bindGroup'expl : (fmap2 impl'name bindGroup'impl)

    freeVarsBg = fold


orderDeps :: FunMap -> TypeMap -> Deps -> ParseResult FunOrder
orderDeps = undefined

renderToBinds :: FunMap -> TypeMap -> FunOrder -> ParseResult [BindGroup Lang]
renderToBinds funs tys names = mapM toGroup names
  where
    toGroup name = case Map.lookup name funs of
      Nothing  -> parseFailedVar "Undefined variable" name
      Just f   -> return $ case Map.lookup name tys of
                    Nothing  -> implGroup name f
                    Just ty  -> explGroup name f ty


    implGroup name f = BindGroup [] [[Impl (fromVarName name) [f]]]

    explGroup name f ty = BindGroup [Expl (fromVarName name) ty [f]] []






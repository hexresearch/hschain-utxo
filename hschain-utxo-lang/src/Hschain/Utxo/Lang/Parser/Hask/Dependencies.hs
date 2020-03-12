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

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Lib.Base
import Hschain.Utxo.Lang.Parser.Hask.Utils

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as Text
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Language.HM as H

type TypeMap  = Map VarName Signature
type FunMap   = Map VarName (Alt Lang)

data Decl
  = FunDecl Loc [(VarName, Alt Lang)]
  | TypeSig Loc [VarName] Signature

toBindGroup :: [Decl] -> ParseResult (BindGroup Lang)
toBindGroup = fmap sortBindGroups . parseBinds

parseBinds :: [Decl] -> ParseResult (BindGroup Lang)
parseBinds ds = do
  typeMap <- getTypeMap ds
  funMap  <- getFunMap ds
  renderToBinds funMap typeMap

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

renderToBinds :: FunMap -> TypeMap -> ParseResult (BindGroup Lang)
renderToBinds funs tys = mapM toGroup names
  where
    names = L.nub $ mappend (Map.keys funs) (Map.keys tys)

    toGroup name = case Map.lookup name funs of
      Nothing  -> parseFailedVar "Undefined variable" name
      Just f   -> return $ case Map.lookup name tys of
                    Nothing  -> implGroup name f
                    Just ty  -> explGroup name f ty


    implGroup name f = Bind name Nothing [f]

    explGroup name f ty = Bind name (Just ty) [f]


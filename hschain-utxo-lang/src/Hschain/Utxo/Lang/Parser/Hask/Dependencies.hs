-- | Module for reordering binds in the module by dependencies.
-- It's dependency sorting. Within the module expressions can be defined in any order
-- but we need to sort them for proper execution.
module Hschain.Utxo.Lang.Parser.Hask.Dependencies(
    Decl(..)
  , toBindGroup
) where


import Control.Monad

import Data.Function (on)
import Data.Map.Strict (Map)
import Data.Maybe

import Language.Haskell.Exts.Parser (
    ParseResult(..))

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.UserType
import Hschain.Utxo.Lang.Parser.Hask.Utils

import qualified Data.Map.Strict as Map
import qualified Data.Text as Text
import qualified Data.List as L

type TypeMap  = Map VarName Signature

-- | Declarations in the module
data Decl
  = FunDecl Loc [(VarName, [Alt Lang])]   -- ^ Definition of the function (or value)
  | PatDecl Loc Pat (Alt Lang)            -- ^ Pattern definition
  | TypeSig Loc [VarName] Signature       -- ^ Type-signature for the value (@val :: Type@)
  | DataDecl UserType                     -- ^ Definition of the type

groupAdjacentFunDecl :: [Decl] -> [Decl]
groupAdjacentFunDecl ds = onFunDecl (fmap joinGroup . L.groupBy sameFunDecl) =<< ds
  where
    onFunDecl f x = case x of
      FunDecl loc as -> fmap (FunDecl loc) (f as)
      other          -> pure $ other

    sameFunDecl = (==) `on` fst

    joinGroup xs = case xs of
      (a:_) -> [(fst a, concat $ fmap snd xs)]
      []    -> []

-- | Sorts declarations and converts them to the list of bindings.
toBindGroup :: [Decl] -> ParseResult (Binds Lang)
toBindGroup = fmap sortBinds . parseBinds . groupAdjacentFunDecl

parseBinds :: [Decl] -> ParseResult (Binds Lang)
parseBinds ds = do
  typeMap <- getTypeMap ds
  let vals = getVals ds
  return $ Binds
      { binds'types = typeMap
      , binds'decls = vals
      }

getVals :: [Decl] -> [Bind Lang]
getVals ds = extractVal =<< ds
  where
    extractVal = \case
      FunDecl _ vs      -> fmap (uncurry FunBind) vs
      PatDecl _ pat alt -> pure $ PatBind pat alt
      _                 -> []


getTypeMap :: [Decl] -> ParseResult TypeMap
getTypeMap = foldM accumTypeMap Map.empty . concat . catMaybes . fmap getTypeSig
  where
    getTypeSig = \case
      TypeSig _ vs ty -> Just $ fmap (\v -> (v, ty)) vs
      _                 -> Nothing

    accumTypeMap m (v, ty) = case Map.lookup v m of
      Nothing -> return $ Map.insert v ty m
      Just _  -> parseFailed (varName'loc v) $ mconcat ["Type signature for variable ", Text.unpack $ varName'name v, " is defined twice"]


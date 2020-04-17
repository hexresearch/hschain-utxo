module Hschain.Utxo.Lang.Desugar.Records(
    removeRecordCons
  , orderRecordFields
  , orderRecordFieldsFromContext
) where

import Control.Arrow (first)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar.PatternCompiler

import Data.Fix

import Language.HM (getLoc, monoT)

import qualified Data.List.Extra as L
import qualified Data.Map.Strict as M
import qualified Data.Vector as V

removeRecordCons :: MonadError Error m => UserTypeCtx -> Lang -> m Lang
removeRecordCons ctx = cataM $ \case
  RecConstr loc cons fields -> fmap (Fix . Cons loc cons . V.fromList) $ orderRecordFieldsFromContext ctx cons fields
  other                     -> return $ Fix other

orderRecordFieldsFromContext ::
     MonadError Error m
  => UserTypeCtx -> ConsName -> [(VarName, Lang)] -> m [Lang]
orderRecordFieldsFromContext UserTypeCtx{..} cons fields =
  maybe err (\order -> orderRecordFields order cons fields) mOrder
  where
    mOrder = M.lookup cons userTypeCtx'recConstrs
    err = throwError $ ExecError $ UndefinedRecordCons (consName'loc cons) cons


orderRecordFields :: MonadError Error m => RecordFieldOrder -> ConsName -> [(VarName, Lang)] -> m [Lang]
orderRecordFields (RecordFieldOrder consOrder) cons fields =
  mapM (toArg m) consOrder
  where
    m = M.fromList $ fmap (first varName'name) fields

    toArg m name = maybe (err name) pure $ M.lookup name m

    err field = throwError $ ExecError $ UndefinedReocrdField (getLoc cons) cons field


-- | Module defines function to desugar record getters and modifiers
-- to simple function applications.
module Hschain.Utxo.Lang.Desugar.Records(
    removeRecordCons
  , orderRecordFieldsFromContext
) where

import Control.Arrow (first)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad

import Data.Fix

import Language.HM (getLoc)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- | Substitutes record constructor application with named fields
-- to ordinary constructor applications.
removeRecordCons :: MonadError Error m => UserTypeCtx -> Lang -> m Lang
removeRecordCons ctx = cataM $ \case
  RecConstr loc cons fields -> fmap (Fix . Cons loc cons . V.fromList) $ orderRecordFieldsFromContext ctx cons fields
  other                     -> return $ Fix other

-- | Sort fields by the order by which they are defined in the type declaration.
orderRecordFieldsFromContext ::
     MonadError Error m
  => UserTypeCtx -> ConsName -> [(VarName, Lang)] -> m [Lang]
orderRecordFieldsFromContext UserTypeCtx{..} cons fields =
  maybe err (\order -> orderRecordFields order cons fields) mOrder
  where
    mOrder = M.lookup cons userTypeCtx'recConstrs
    err = throwError $ ExecError $ UndefinedRecordCons (consName'loc cons) cons

-- | Sort fields by the order by which they are defined in the type declaration.
orderRecordFields :: MonadError Error m => RecordFieldOrder -> ConsName -> [(VarName, Lang)] -> m [Lang]
orderRecordFields (RecordFieldOrder consOrder) cons fields =
  mapM (toArg m) consOrder
  where
    m = M.fromList $ fmap (first varName'name) fields

    toArg mp name = maybe (err name) pure $ M.lookup name mp

    err field = throwError $ ExecError $ UndefinedReocrdField (getLoc cons) cons field


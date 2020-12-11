-- | Module defines function to desugar record getters and modifiers
-- to simple function applications.
module Hschain.Utxo.Lang.Desugar.Records(
    removeRecordCons
  , orderRecordFieldsFromContext
  , removeRecordUpdate
  , removeRecords
) where

import Control.Arrow (first)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad

import Data.Fix
import Data.Maybe

import Type.Check.HM (getLoc)

import qualified Data.Map.Strict as M
import qualified Data.Vector as V

-- | Removes all expressions that involve records.
-- There are two types of expressions: record constructors and record updates.
-- They are substituted with simple constructors and updates with case-expressions.
removeRecords :: MonadError Error m => UserTypeCtx -> Lang -> m Lang
removeRecords ctx = removeRecordCons ctx >=> removeRecordUpdate ctx

-- | Substitutes record constructor application with named fields
-- to ordinary constructor applications.
removeRecordCons :: MonadError Error m => UserTypeCtx -> Lang -> m Lang
removeRecordCons ctx = foldFixM $ \case
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

-- | Substitutes record updates syntax with updates
-- with simple case-expressions
removeRecordUpdate :: MonadError Error m => UserTypeCtx -> Lang -> m Lang
removeRecordUpdate ctx = foldFixM $ \case
  RecUpdate loc expr fieldUpdates -> do
    mCaseExpr <- toRecUpdateCaseExpr ctx fieldUpdates
    return $ case mCaseExpr of
      Nothing  -> expr
      Just alt -> Fix $ CaseOf loc expr [alt]
  other -> pure $ Fix other

toRecUpdateCaseExpr :: MonadError Error m
  => UserTypeCtx -> [(VarName, Lang)] -> m (Maybe (CaseExpr Lang))
toRecUpdateCaseExpr ctx fieldUpdates = case fieldUpdates of
  [] -> return Nothing
  (field, _):_ -> fmap (Just . toCaseExpr) $ maybe (notFound field) pure $
      M.lookup (varName'name field) $ userTypeCtx'recFields ctx
  where
    toCaseExpr (cons, RecordFieldOrder fields) = CaseExpr
      { caseExpr'lhs = PCons noLoc cons $ fmap (PVar noLoc . VarName noLoc) fields
      , caseExpr'rhs = Fix $ Cons noLoc cons $ V.fromList $ fmap substField fields
      }

    fieldUpdateMap = M.fromList $ fmap (first varName'name) fieldUpdates

    substField field = fromMaybe (Fix $ Var noLoc $ VarName noLoc field) $ M.lookup field fieldUpdateMap

    notFound field = throwError $ ExecError $
      UndefinedReocrdField (getLoc field) (varToConsName field) (varName'name field)



module Hschain.Utxo.Lang.Desugar(
    desugar
  , unfoldLamList
  , unfoldLetArg
  , unfoldInfixApply
  , singleLet
  , app1
  , app2
  , app3
  , bindBodyToExpr
  , bindGroupToLet
  , simpleBind
  , caseToLet
  , reduceSubPats
  , desugarRecordUpdate
  , recordFieldUpdateFunName
  , secretVar
  , module Hschain.Utxo.Lang.Desugar.FreshVar
  , module Hschain.Utxo.Lang.Desugar.PatternCompiler
  , module Hschain.Utxo.Lang.Desugar.Records
) where

import Hex.Common.Control

import Control.Applicative
import Control.Arrow (first)
import Control.Monad.State.Strict
import Control.Monad.Extra (firstJustM)

import Data.Fix
import Data.Map.Strict (Map)
import Data.Text (Text)

import Language.HM (getLoc, stripSignature, monoT)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar.FreshVar
import Hschain.Utxo.Lang.Desugar.Guard
import Hschain.Utxo.Lang.Desugar.PatternCompiler
import Hschain.Utxo.Lang.Desugar.Records

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.List.Extra as L


desugar :: MonadLang m => UserTypeCtx -> Lang -> m Lang
desugar ctx expr = removeRecordCons ctx expr

unfoldLamList :: Loc -> [Pat] -> Lang -> Lang
unfoldLamList loc pats a = L.foldl' (\z a -> z . Fix . Lam loc a) id pats a

unfoldLetArg :: Loc -> VarName -> [VarName] -> Lang -> Lang -> Lang
unfoldLetArg loc v args a = singleLet loc v (Fix $ LamList loc (fmap varToPat args) a)

varToPat :: VarName -> Pat
varToPat v = PVar (getLoc v) v

singleLet :: Loc -> VarName -> Lang -> Lang -> Lang
singleLet loc v body expr = Fix $ Let loc [simpleBind v body] expr

unfoldInfixApply :: Loc -> Lang -> VarName -> Lang -> Lang
unfoldInfixApply loc a v b = app2 (Fix $ Var loc v) a b

bindGroupToLet :: BindGroup Lang -> Lang -> Lang
bindGroupToLet bgs expr = Fix $ Let noLoc bgs expr


app1 :: Lang -> Lang -> Lang
app1 f a = Fix (Apply (getLoc a) f a)

app2 :: Lang -> Lang -> Lang -> Lang
app2 f a b = Fix (Apply (getLoc f) (Fix (Apply (getLoc a) f a)) b)

app3 :: Lang -> Lang -> Lang -> Lang -> Lang
app3 f a b c = Fix $ Apply (getLoc f) (app2 f a b) c

bindBodyToExpr :: MonadLang m => Bind Lang -> m Lang
bindBodyToExpr Bind{..} = fmap addSignatureCheck $ altGroupToExpr bind'alts
  where
    addSignatureCheck = maybe id (\ty x -> Fix $ Ascr (getLoc ty) x ty) bind'type

simpleBind :: VarName -> Lang -> Bind Lang
simpleBind v a = Bind v Nothing [Alt [] (UnguardedRhs a) Nothing]

-----------------------------------------------------------------
caseToLet :: MonadLang m =>
  (ConsName -> Int -> Text) -> Loc -> Lang -> [CaseExpr Lang] -> m Lang
caseToLet toSelectorName loc expr cases = do
  v <- getFreshVar loc
  fmap (Fix . Let loc [simpleBind v expr]) $ caseToLet' toSelectorName loc v cases

caseToLet' :: MonadLang m =>
  (ConsName -> Int -> Text) -> Loc -> VarName -> [CaseExpr Lang] -> m Lang
caseToLet' toSelectorName topLoc var cases = fmap (foldr (\(loc, a) rest -> Fix $ AltE loc a rest) failCase) $ mapM fromCase cases
  where
    toVarExpr loc v = Fix $ Var loc $ VarName loc $ varName'name v

    fromCase CaseExpr{..} = fmap (getLoc caseExpr'lhs, ) $ case caseExpr'lhs of
      PVar ploc pvar -> return $ Fix $ Let ploc [simpleBind pvar $ toVarExpr ploc var] caseExpr'rhs
      PWildCard loc -> return $ caseExpr'rhs
      PPrim ploc p -> return $ Fix $ If ploc (eqPrim ploc var p) caseExpr'rhs failCase
      PCons ploc cons pats ->
        case pats of
          [] -> constCons ploc cons
          _  -> argCons ploc cons pats
      PTuple ploc pats -> do
        (vs, rhs') <- reduceSubPats pats caseExpr'rhs
        let size = length vs
            bg = zipWith (\n v -> simpleBind v (Fix $ UnOpE (varName'loc v) (TupleAt size n) $ toVarExpr ploc var)) [0..] vs
        return $ Fix $ Let ploc bg rhs'
      where
        constCons ploc cons = return $
          app2 (Fix $ Var ploc $ VarName ploc $ toSelectorName cons 0) (toVarExpr ploc var) caseExpr'rhs

        argCons ploc cons pats = do
          (vs, rhs') <- reduceSubPats pats caseExpr'rhs
          let bg = zipWith (\n v -> simpleBind v (Fix $ Apply (varName'loc v) (selector ploc cons n) $ toVarExpr ploc var)) [0..] vs
          return $ Fix $ Let ploc bg rhs'

    selector ploc cons n = Fix $ Var ploc (VarName ploc (toSelectorName cons n))

    failCase = Fix $ FailCase topLoc

    eqPrim ploc v p = Fix $ BinOpE ploc Equals (toVarExpr ploc var) (Fix $ PrimE ploc p)

reduceSubPats :: forall m . MonadLang m => [Pat] -> Lang -> m ([VarName], Lang)
reduceSubPats pats rhs = runStateT (mapM go pats) rhs
  where
    go :: Pat -> StateT Lang m VarName
    go pat = case pat of
      PVar _ var -> return var
      _          -> do
        expr <- get
        let loc = getLoc pat
        var  <- lift $ getFreshVar loc
        put $ Fix $ CaseOf loc (Fix $ Var loc var) $ [CaseExpr pat expr]
        return var

desugarRecordUpdate :: VarName -> Lang -> Lang -> Lang
desugarRecordUpdate field val expr =
  app2 (Fix $ Var (getLoc field) $ recordFieldUpdateFunName field) val expr

recordFieldUpdateFunName :: VarName -> VarName
recordFieldUpdateFunName VarName{..} = VarName
  { varName'loc  = varName'loc
  , varName'name = secretVar $ mappend "update_" varName'name
  }

secretVar :: Text -> Text
secretVar = flip mappend "___"


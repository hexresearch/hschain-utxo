module Hschain.Utxo.Lang.Desugar(
    unfoldLamList
  , unfoldLetArg
  , unfoldInfixApply
  , singleLet
  , app1
  , app2
  , app3
  , moduleToMainExpr
  , bindGroupToLet
  , bindBodyToExpr
  , simpleBind
  , caseToLet
  , reduceSubPats
  , module Hschain.Utxo.Lang.Desugar.FreshVar
  , module Hschain.Utxo.Lang.Desugar.PatternCompiler
) where

import Hex.Common.Control

import Control.Applicative
import Control.Monad.State.Strict
import Control.Monad.Extra (firstJustM)

import Data.Fix
import Data.Text (Text)

import Language.HM (getLoc, stripSignature, monoT)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar.FreshVar
import Hschain.Utxo.Lang.Desugar.Guard
import Hschain.Utxo.Lang.Desugar.PatternCompiler

import qualified Data.List as L
import qualified Data.List.Extra as L


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

moduleToMainExpr :: MonadLang m => Module -> m Lang
moduleToMainExpr prog = case findMain prog of
  Nothing   -> throwError $ ExecError NoMainFunction
  Just main -> fmap (\mainExpr -> bindGroupToLet (module'binds $ rmMain prog) (addBoolTypeCheck mainExpr)) $ altGroupToExpr main
  where
    findMain :: Module -> Maybe [Alt Lang]
    findMain Module{..} = L.firstJust getMain module'binds
      where
        getMain Bind{..}
          | isMain bind'name = Just bind'alts
          | otherwise        = Nothing

    addBoolTypeCheck :: Lang -> Lang
    addBoolTypeCheck expr = Fix $ Ascr (getLoc expr) expr (monoT boolT)

    rmMain :: Module -> Module
    rmMain m@Module{..} = m { module'binds = rm module'binds }
      where
        rm = filter noMain

        noMain = not . isMain . bind'name

    isMain :: VarName -> Bool
    isMain = (== "main") . varName'name


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
caseToLet toSelectorName loc var cases = fmap (foldr (\a rest -> Fix $ AltE loc a rest) failCase) $ mapM fromCase cases
  where
    fromCase CaseExpr{..} = case caseExpr'lhs of
      PVar ploc pvar -> return $ Fix $ Let ploc [simpleBind pvar var] caseExpr'rhs
      PWildCard loc -> return $ caseExpr'rhs
      PPrim ploc p -> return $ Fix $ If ploc (eqPrim ploc var p) caseExpr'rhs failCase
      PCons ploc cons pats -> do
        (vs, rhs') <- reduceSubPats pats caseExpr'rhs
        let bg = zipWith (\n v -> simpleBind v (Fix $ Apply (varName'loc v) (selector ploc cons n) var)) [0..] vs
        return $ Fix $ Let loc bg rhs'
      PTuple ploc pats -> do
        (vs, rhs') <- reduceSubPats pats caseExpr'rhs
        let size = length vs
            bg = zipWith (\n v -> simpleBind v (Fix $ UnOpE (varName'loc v) (TupleAt size n) $ var)) [0..] vs
        return $ Fix $ Let loc bg rhs'

    selector ploc cons n = Fix $ Var ploc (VarName ploc (toSelectorName cons n))

    failCase = Fix $ FailCase loc

    eqPrim ploc v p = Fix $ BinOpE ploc Equals var (Fix $ PrimE ploc p)

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


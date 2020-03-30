module Hschain.Utxo.Lang.Desugar(
    unfoldLamList
  , unfoldLetArg
  , unfoldInfixApply
  , singleLet
  , app1
  , app2
  , app3
  , altToExpr
  , moduleToMainExpr
  , bindGroupToLet
  , bindBodyToExpr
  , simpleBind
  , caseToLet
) where

import Control.Applicative
import Control.Monad.State.Strict

import Data.Fix
import Data.Text (Text)

import Language.HM (getLoc, stripSignature, monoT)

import Hschain.Utxo.Lang.Expr

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

moduleToMainExpr :: Module -> Either String Lang
moduleToMainExpr prog = case findMain prog of
  Nothing   -> Left "There is no main expression defined in the module"
  Just main -> Right $ bindGroupToLet (module'binds $ rmMain prog) (addBoolTypeCheck main)
  where
    findMain :: Module -> Maybe Lang
    findMain Module{..} = L.firstJust getMain module'binds
      where
        getMain Bind{..}
          | isMain bind'name = Just $ altToExpr bind'alt
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

altToExpr :: Alt Lang -> Lang
altToExpr Alt{..} = foldr toArg alt'expr alt'pats
  where
    toArg pat body = Fix $ Lam (getLoc pat) pat body

bindBodyToExpr :: Bind Lang -> Lang
bindBodyToExpr Bind{..} = addSignatureCheck $ altToExpr bind'alt
  where
    addSignatureCheck = maybe id (\ty x -> Fix $ Ascr (getLoc ty) x ty) bind'type

simpleBind :: VarName -> Lang -> Bind Lang
simpleBind v a = Bind v Nothing (Alt [] a)

caseToLet :: (ConsName -> Int -> Text) -> Loc -> Lang -> [CaseExpr Lang] -> Lang
caseToLet toSelectorName loc var cases = foldr (\a rest -> Fix $ AltE loc a rest) failCase $ fmap fromCase cases
  where
    fromCase CaseExpr{..} = case caseExpr'lhs of
      PVar ploc pvar -> Fix $ Let ploc [simpleBind pvar var] caseExpr'rhs
      PWildCard loc -> caseExpr'rhs
      PPrim ploc p -> Fix $ If ploc (eqPrim ploc var p) caseExpr'rhs failCase
      PCons ploc cons vs ->
          let bg = zipWith (\n v -> simpleBind v (Fix $ Apply (varName'loc v) (selector ploc cons n) var)) [0..] vs
          in  Fix $ Let loc bg caseExpr'rhs
      PTuple ploc vs ->
          let size = length vs
              bg = zipWith (\n v -> simpleBind v (Fix $ UnOpE (varName'loc v) (TupleAt size n) $ var)) [0..] vs
          in  Fix $ Let loc bg caseExpr'rhs

    selector ploc cons n = Fix $ Var ploc (VarName ploc (toSelectorName cons n))

    failCase = Fix $ FailCase loc

    eqPrim ploc v p = Fix $ BinOpE ploc Equals var (Fix $ PrimE ploc p)



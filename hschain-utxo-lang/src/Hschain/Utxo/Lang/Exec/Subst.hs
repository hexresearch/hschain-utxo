-- | Algorithm of expression substitutions
module Hschain.Utxo.Lang.Exec.Subst(
  subst
) where

import Control.Arrow (second)

import Data.Fix

import Hschain.Utxo.Lang.Expr

import qualified Data.Set as S

-- | Substitute in the expression any free-variables with given name by subexpression.
--
-- > subst expr var subExpr
--
-- It substitutes in the @expr@ all @var@ to @subExpr@.
subst :: Lang -> VarName -> Lang -> Lang
subst (Fix body) varName sub = case body of
  Var loc e                | e == varName  -> sub
                           | otherwise     -> Fix $ Var loc e
  PrimE loc p                              -> Fix $ PrimE loc p
  Ascr loc lc t                            -> Fix $ Ascr loc (rec lc) t
  Apply loc a b                            -> Fix $ Apply loc (rec a) (rec b)
  InfixApply loc a v b     | v == varName  -> subInfix loc sub a b
  InfixApply loc a v b     | otherwise     -> Fix $ InfixApply loc (rec a) v (rec b)
  Lam loc pat body1                        -> Fix $ Lam loc pat $ recBy (freeVarsPat pat) body1
  If loc cond t e                          -> Fix $ If loc (rec cond) (rec t) (rec e)
  Let loc bg e                             -> Fix $ Let loc (substBindGroup bg) (recBy (getBindsNames bg) e)
  PrimLet loc bg e                         -> Fix $ PrimLet loc (substPrimBindGeoup bg) (recBy (primBindVars bg) e)
  Tuple loc as                             -> Fix $ Tuple loc $ fmap rec as
  List loc as                              -> Fix $ List loc $ fmap rec as
  NegApp loc a                             -> Fix $ NegApp loc $ rec a
  LamList loc vs a                         -> Fix $ LamList loc vs $ recBy (foldMap freeVarsPat vs) a
  FailCase loc                             -> Fix $ FailCase loc
  AltE loc a b                             -> Fix $ AltE loc (rec a) (rec b)
  CaseOf loc expr cases                    -> Fix $ CaseOf loc (rec expr) (fmap substCase cases)
  Cons loc name vs                         -> Fix $ Cons loc name $ fmap rec vs
  RecConstr loc name vals                  -> Fix $ RecConstr loc name (fmap (second rec) vals)
  RecUpdate loc a upds                     -> Fix $ RecUpdate loc (rec a) (fmap (second rec) upds)
  AntiQuote loc ty v                       -> Fix $ AntiQuote loc ty v
  where
    subInfix loc op a b = rec $ Fix (Apply loc (Fix $ Apply loc op a) b)

    rec x = subst x varName sub

    recBy vars x
      | S.member varName vars = x
      | otherwise             = subst x varName sub

    substBindGroup bs = bs { binds'decls = fmap substBind $ binds'decls bs }

    substPrimBindGeoup = fmap (second rec)

    substBind x = case x of
      FunBind{..} | isBoundName bind'name -> x
      FunBind{..}                         -> x { bind'alts = fmap substAlt bind'alts }
      PatBind{..} | isBoundPat  bind'pat  -> x
      PatBind{..}                         -> x { bind'alt = substAlt bind'alt }
      where
        isBoundName name = varName == name
        isBoundPat pat = S.member varName (freeVarsPat pat)


    substAlt x@Alt{..}
      | isBinded varName alt'pats = x
      | otherwise                 = x { alt'expr  = fmap rec alt'expr
                                      , alt'where = fmap substBindGroup alt'where }

      where
        isBinded v ps = v `elem` (foldMap freeVarsPat ps)

    primBindVars = S.fromList . fmap fst

    substCase x@CaseExpr{..} = fmap (recBy binds) x
      where
        binds = freeVarsPat caseExpr'lhs

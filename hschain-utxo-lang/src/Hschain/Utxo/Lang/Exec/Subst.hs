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
  UnOpE loc uo lc                          -> Fix $ UnOpE loc uo $ rec lc
  BinOpE loc bo a b                        -> Fix $ BinOpE loc bo (rec a) (rec b)
  Apply loc a b                            -> Fix $ Apply loc (rec a) (rec b)
  InfixApply loc a v b     | v == varName  -> subInfix loc sub a b
  InfixApply loc a v b     | otherwise     -> Fix $ InfixApply loc (rec a) v (rec b)
  Lam loc pat body1                        -> Fix $ Lam loc pat $ recBy (freeVarsPat pat) body1
  If loc cond t e                          -> Fix $ If loc (rec cond) (rec t) (rec e)
  Let loc bg e                             -> Fix $ Let loc (substBindGroup bg) (recBy (bindVars bg) e)
  PrimLet loc bg e                         -> Fix $ PrimLet loc (substPrimBindGeoup bg) (recBy (primBindVars bg) e)
  Tuple loc as                             -> Fix $ Tuple loc $ fmap rec as
  GetEnv loc idx                           -> Fix $ GetEnv loc $ fmap rec idx
  SigmaE loc sigma                         -> Fix $ SigmaE loc $ fmap rec sigma
  VecE loc vec                             -> Fix $ VecE loc $ fmap rec vec
  TextE loc txt                            -> Fix $ TextE loc $ fmap rec txt
  BytesE loc txt                           -> Fix $ BytesE loc $ fmap rec txt
  BoxE loc box                             -> Fix $ BoxE loc $ fmap rec box
  LamList loc vs a                         -> Fix $ LamList loc vs $ recBy (foldMap freeVarsPat vs) a
  Trace loc a b                            -> Fix $ Trace loc (rec a) (rec b)
  FailCase loc                             -> Fix $ FailCase loc
  AltE loc a b                             -> Fix $ AltE loc (rec a) (rec b)
  CaseOf loc expr cases                    -> Fix $ CaseOf loc (rec expr) (fmap substCase cases)
  Cons loc name vs                         -> Fix $ Cons loc name $ fmap rec vs
  RecConstr loc name vals                  -> Fix $ RecConstr loc name (fmap (second rec) vals)
  RecUpdate loc a upds                     -> Fix $ RecUpdate loc (rec a) (fmap (second rec) upds)
  where
    subInfix loc op a b = rec $ Fix (Apply loc (Fix $ Apply loc op a) b)

    rec x = subst x varName sub

    recBy vars x
      | S.member varName vars = x
      | otherwise             = subst x varName sub

    substBindGroup = fmap substBind

    substPrimBindGeoup = fmap (second rec)

    substBind x@Bind{..}
      | varName == bind'name = x
      | otherwise            = x { bind'alts = fmap substAlt bind'alts }

    substAlt x@Alt{..}
      | isBinded varName alt'pats = x
      | otherwise                 = x { alt'expr  = fmap rec alt'expr
                                      , alt'where = fmap substBindGroup alt'where }

      where
        isBinded v ps = v `elem` (foldMap freeVarsPat ps)

    bindVars = S.fromList . fmap bind'name

    primBindVars = S.fromList . fmap fst

    substCase x@CaseExpr{..} = fmap (recBy binds) x
      where
        binds = freeVarsPat caseExpr'lhs

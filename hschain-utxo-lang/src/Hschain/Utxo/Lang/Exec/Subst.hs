module Hschain.Utxo.Lang.Exec.Subst(
  subst
) where

import Data.Fix

import Hschain.Utxo.Lang.Expr

import qualified Data.Set as S

subst :: Lang -> VarName -> Lang -> Lang
subst (Fix body) varName sub = case body of
  Var loc e                | e == varName  -> sub
                           | otherwise     -> Fix $ Var loc e
  PrimE _ p                                -> Fix body
  Ascr loc lc t                            -> Fix $ Ascr loc (rec lc) t
  UnOpE loc uo lc                          -> Fix $ UnOpE loc uo $ rec lc
  BinOpE loc bo a b                        -> Fix $ BinOpE loc bo (rec a) (rec b)
  Apply loc a b                            -> Fix $ Apply loc (rec a) (rec b)
  InfixApply loc a v b     | v == varName  -> subInfix loc sub a b
  InfixApply loc a v b     | otherwise     -> Fix $ InfixApply loc (rec a) v (rec b)
  Lam loc pat body1                        -> Fix $ Lam loc pat $ recBy (freeVarsPat pat) body1
  If loc cond t e                          -> Fix $ If loc (rec cond) (rec t) (rec e)
  Let loc bg e                             -> Fix $ Let loc (substBindGroup bg) (recBy (bindVars bg) e)
  LetRec loc v1 a1 a2      | v1 == varName -> Fix $ LetRec loc v1 a1 (rec a2)
                           | otherwise     -> Fix $ LetRec loc v1 (rec a1) (rec a2)
  Pk loc a                                 -> Fix $ Pk loc $ rec a
  Tuple loc as                             -> Fix $ Tuple loc $ fmap rec as
  GetEnv loc idx                           -> Fix $ GetEnv loc $ fmap rec idx
  VecE loc vec                             -> Fix $ VecE loc $ fmap rec vec
  TextE loc txt                            -> Fix $ TextE loc $ fmap rec txt
  BoxE loc box                             -> Fix $ BoxE loc $ fmap rec box
  LamList loc vs a                         -> Fix $ LamList loc vs $ recBy (foldMap freeVarsPat vs) a
  Trace loc a b                            -> Fix $ Trace loc (rec a) (rec b)
  FailCase loc                             -> Fix $ FailCase loc
  AltE loc a b                             -> Fix $ AltE loc (rec a) (rec b)
  CaseOf loc expr cases                    -> Fix $ CaseOf loc (rec expr) (fmap substCase cases)
  Cons loc name vs                         -> Fix $ Cons loc name $ fmap rec vs
  where
    subInfix loc op a b = rec $ Fix (Apply loc (Fix $ Apply loc op a) b)

    rec x = subst x varName sub

    recBy bindVars x
      | S.member varName bindVars = x
      | otherwise                   = subst x varName sub

    substBindGroup = fmap substBind

    substBind x@Bind{..}
      | varName == bind'name = x
      | otherwise            = x { bind'alts = fmap substAlt bind'alts }

    substAlt x@Alt{..}
      | isBinded varName alt'pats = x
      | otherwise                 = x{ alt'expr = rec alt'expr }
      where
        isBinded v ps = v `elem` (foldMap freeVarsPat ps)

    bindVars = S.fromList . fmap bind'name

    substCase x@CaseExpr{..} = fmap (recBy binds) x
      where
        binds = freeVarsPat caseExpr'lhs

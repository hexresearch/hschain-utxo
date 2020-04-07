{-# OPTIONS_GHC -Wno-orphans #-}
module Language.HM.Pretty(
) where

import Data.Bool
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Language.HM.Type
import Language.HM.Term

import qualified Data.Map.Strict as Map

instance Pretty (Signature src) where
  pretty = cata go . unSignature
    where
      go = \case
        ForAllT _ _ r -> r
        MonoT ty      -> pretty ty

instance Pretty (Type src) where
  pretty = go False initEnv initCtx . unType
    where
      go :: Bool -> FixityEnv -> FixityContext -> Fix (TypeF src) -> Doc ann
      go isArrPrev env ctx (Fix expr) = case expr of
        VarT _ name   -> pretty name
        ConT _ name   -> pretty name
        ArrowT _ a b  -> fromBin "->" a b
        AppT  _ f a   -> fromAp isArrPrev f a
        where
          fromAp isArr (Fix f) a = case f of
            AppT _ (Fix (ConT _ op)) b
              | env `contains` Op op -> fromBin op b a
            _ -> fromUn isArr (Fix f) a

          fromUn isArr f a = maybeParens (not isArr && needsParens env ctx OpFunAp) $ hsep $
            go False env (FcLeft OpFunAp) unOp :
            fmap (go False env (FcRight OpFunAp)) args
            where
              (unOp, args) = getApArgs f a


          fromBin op a b = maybeParens (needsParens env ctx (Op op)) $ hsep
            [ go True env (FcLeft $ Op op) a
            , pretty op
            , go True env (FcRight $ Op op) b
            ]

          -- Collects left-associative args
          getApArgs fun a = loop fun [a]
            where
              loop (Fix f) res = case f of
                AppT _ g b -> loop g (b:res)
                _          -> (Fix f, res)

      initCtx = FcNone

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens cond = bool id parens cond

needsParens :: FixityEnv -> FixityContext -> Operator -> Bool
needsParens env = \case
  FcNone      -> const False
  FcLeft ctx  -> fcLeft ctx
  FcRight ctx -> fcRight ctx
  where
    fcLeft ctxt op
      | comparePrec env ctxt op == PoLT = False
      | comparePrec env ctxt op == PoGT = True
      | comparePrec env ctxt op == PoNC = True
      -- otherwise the two operators have the same precedence
      | fixity env ctxt /= fixity env op = True
      | fixity env ctxt == FixLeft = False
      | otherwise = True

    fcRight ctxt op
      | comparePrec env ctxt op == PoLT = False
      | comparePrec env ctxt op == PoGT = True
      | comparePrec env ctxt op == PoNC = True
      -- otherwise the two operators have the same precedence
      | fixity env ctxt /= fixity env op = True
      | fixity env ctxt == FixRight = False
      | otherwise = True

data PartialOrdering = PoLT | PoGT | PoEQ | PoNC
  deriving Eq

data OpFix = OpFix
  { opFix'fixity :: !Fixity
  , opFix'prec   :: !Int
  }

data Fixity = FixLeft | FixRight | FixNone
  deriving Eq

data Operator = OpFunAp | Op Text
  deriving (Eq, Ord)

data FixityContext = FcNone | FcLeft Operator | FcRight Operator

type FixityEnv = Map Operator OpFix

initEnv :: FixityEnv
initEnv = Map.fromList
  [ (Op "->", OpFix FixRight 2) ]

contains :: FixityEnv -> Operator -> Bool
contains m op = isJust $ Map.lookup op m

comparePrec :: FixityEnv -> Operator -> Operator -> PartialOrdering
comparePrec env a b = case (Map.lookup a env, Map.lookup b env) of
  (Just opA, Just opB) -> toPo (opFix'prec opA) (opFix'prec opB)
  _                    -> PoNC
  where
    toPo m n
      | m < n     = PoLT
      | m > n     = PoGT
      | otherwise = PoEQ


fixity :: FixityEnv -> Operator -> Fixity
fixity env op = maybe FixNone opFix'fixity $ Map.lookup op env

---------------------------------------

instance Pretty (Term src) where
  pretty (Term x) = cata prettyTermF x
    where
      prettyTermF = \case
        Var _ v      -> pretty v
        App _ a b    -> parens $ hsep [a, b]
        Abs _ v a    -> parens $ hsep [hcat ["\\", pretty v], "->", a]
        Let _ v a b  -> vcat [ hsep ["let", pretty v, "=", a]
                             , hsep ["in ", b]]
        AssertType _ r sig -> parens $ hsep [r, "::", pretty sig]





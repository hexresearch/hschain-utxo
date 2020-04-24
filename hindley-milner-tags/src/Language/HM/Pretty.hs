{-# OPTIONS_GHC -Wno-orphans #-}
module Language.HM.Pretty(
    HasPrefix(..)
  , OpFix(..)
  , Fixity(..)
) where

import Control.Monad

import Data.Bool
import Data.Fix
import Data.Maybe
import Data.Text.Prettyprint.Doc

import Language.HM.Type
import Language.HM.Term

class IsVar v => HasPrefix v where
  getFixity :: v -> Maybe OpFix

isPrefix :: HasPrefix v => v -> Bool
isPrefix = isNothing . getFixity

isInfix :: HasPrefix v => v -> Bool
isInfix  = not . isPrefix

instance (Pretty v, HasPrefix v) => Pretty (Signature v) where
  pretty = cata go . unSignature
    where
      go = \case
        ForAllT _ r -> r
        MonoT ty      -> pretty ty

instance (HasPrefix v, Pretty v) => Pretty (Type v) where
  pretty = go False initCtx . unType
    where
      go :: Bool -> FixityContext v -> Fix (TypeF v) -> Doc ann
      go isArrPrev ctx (Fix expr) = case expr of
        VarT name   -> pretty name
        ConT name [a, b] | isInfix name -> fromBin name a b
        ConT name as -> fromCon isArrPrev name as
        where
          fromCon isArr name args = maybeParens (not isArr && needsParens ctx OpFunAp) $ hsep $
            pretty name :
            fmap (go False (FcRight OpFunAp)) args

          fromBin op a b = maybeParens (needsParens ctx (Op op)) $ hsep
            [ go True (FcLeft $ Op op) a
            , pretty op
            , go True (FcRight $ Op op) b
            ]

      initCtx = FcNone

maybeParens :: Bool -> Doc ann -> Doc ann
maybeParens cond = bool id parens cond

needsParens :: HasPrefix v => FixityContext v -> Operator v -> Bool
needsParens = \case
  FcNone      -> const False
  FcLeft ctx  -> fcLeft ctx
  FcRight ctx -> fcRight ctx
  where
    fcLeft ctxt op
      | comparePrec ctxt op == PoLT = False
      | comparePrec ctxt op == PoGT = True
      | comparePrec ctxt op == PoNC = True
      -- otherwise the two operators have the same precedence
      | fixity ctxt /= fixity op = True
      | fixity ctxt == FixLeft = False
      | otherwise = True

    fcRight ctxt op
      | comparePrec ctxt op == PoLT = False
      | comparePrec ctxt op == PoGT = True
      | comparePrec ctxt op == PoNC = True
      -- otherwise the two operators have the same precedence
      | fixity ctxt /= fixity op = True
      | fixity ctxt == FixRight = False
      | otherwise = True

data PartialOrdering = PoLT | PoGT | PoEQ | PoNC
  deriving Eq

data OpFix = OpFix
  { opFix'fixity :: !Fixity
  , opFix'prec   :: !Int
  }

data Fixity = FixLeft | FixRight | FixNone
  deriving Eq

data Operator v = OpFunAp | Op v
  deriving (Eq, Ord)

data FixityContext v = FcNone | FcLeft (Operator v) | FcRight (Operator v)

{-
initEnv :: FixityEnv
initEnv = Map.fromList
  [ (Op "->", OpFix FixRight 2) ]
-}

getFixityEnv :: HasPrefix v => Operator v -> Maybe OpFix
getFixityEnv = getFixity <=< fromOp

fromOp :: Operator v -> Maybe v
fromOp = \case
  OpFunAp -> Nothing
  Op v    -> Just v



comparePrec :: HasPrefix v => Operator v -> Operator v -> PartialOrdering
comparePrec a b = case (getFixityEnv a, getFixityEnv b) of
  (Just opA, Just opB) -> toPo (opFix'prec opA) (opFix'prec opB)
  _                    -> PoNC
  where
    toPo m n
      | m < n     = PoLT
      | m > n     = PoGT
      | otherwise = PoEQ


fixity :: HasPrefix v => Operator v -> Fixity
fixity op = maybe FixNone opFix'fixity $ getFixityEnv op

---------------------------------------

instance (HasPrefix v, Pretty v) => Pretty (Term v) where
  pretty (Term x) = cata prettyTermF x
    where
      prettyTermF = \case
        Var v       -> pretty v
        App a b     -> parens $ hsep [a, b]
        Lam v a     -> parens $ hsep [hcat ["\\", pretty v], "->", a]
        Let vs a    -> onLet vs a
        LetRec vs a -> onLet vs a
        AssertType r sig -> parens $ hsep [r, "::", pretty sig]
        where
          onLet vs body =
            vcat [ hsep ["let", indent 4 $ vcat $ fmap (\(v, a) -> hsep [pretty v, "=", a]) vs]
                 , hsep ["in ", body]]


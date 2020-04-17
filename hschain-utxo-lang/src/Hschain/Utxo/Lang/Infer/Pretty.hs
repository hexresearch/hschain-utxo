module Hschain.Utxo.Lang.Infer.Pretty(
    prettySignature
  , prettyType
) where

import Data.Bool
import Data.Char
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Expr hiding (Var, Let, Type, Signature)

import Language.HM.Type
import Language.HM.Term

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- | Type errors.
data TypeError src
    = OccursErr src Text (Type src)
    | UnifyErr src (Type src) (Type src)
    | NotInScopeErr src Text
    deriving (Eq, Show)

prettySignature :: Signature src -> Doc ann
prettySignature = cata go . unSignature
  where
    go = \case
      ForAllT _ _ r -> r
      MonoT ty      -> prettyType ty

prettyType :: Type src -> Doc ann
prettyType = go False initEnv initCtx . unType
  where
    go :: Bool -> FixityEnv -> FixityContext -> Fix (TypeF src) -> Doc ann
    go isArrPrev env ctx (Fix expr) = case expr of
      VarT _ name   -> pretty name
      ConT _ name   -> pretty name
      ArrowT _ a b  -> fromBin "->" a b
      AppT  _ f a   ->
        case getTupleCon f a of
          Nothing     -> fromAp isArrPrev f a
          Just args   -> fromTuple args
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

        fromTuple args = parens $ hsep $ punctuate comma $ fmap (prettyType . Type) args

        getTupleCon f a = case go f of
          Just args -> Just $ args ++ [a]
          Nothing   -> Nothing
          where
            go (Fix x) = case x of
              ConT _ name | isTupleName name -> Just []
              AppT _ con arg                 -> fmap (++ [arg]) $ go con
              _                              -> Nothing

        isTupleName name = (pre == "Tuple") && isInt post
          where
            (pre, post) = T.splitAt 5 name
            isInt = T.all isDigit


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



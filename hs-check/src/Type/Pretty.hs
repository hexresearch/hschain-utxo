module Type.Pretty where

import Data.Bool
import Data.Char
import Data.Text (Text)
import Data.Text.Prettyprint.Doc
import Data.Text.Prettyprint.Doc.Render.Text (renderStrict)
import Data.Map.Strict (Map)
import Data.Maybe

import Type.Type

import qualified Data.Map.Strict as Map
import qualified Data.Text as T

instance Pretty Scheme where
  pretty (Forall _ _ qual) = pretty qual

instance Pretty a => Pretty (Qual a) where
  pretty (Qual _ ps ty)
    | null ps   = pretty ty
    | otherwise = hsep [parens $ hsep $ punctuate comma $ fmap pretty ps, "=>", pretty ty ]

instance Pretty Pred where
  pretty (IsIn _ idx ty) = hsep [pretty idx, pretty ty]

instance Pretty Id where
  pretty (Id _ txt) = pretty txt

instance Pretty Type where
  pretty = go initEnv initCtx
    where
      go env ctx = \case
        TVar _ (Tyvar _ name _) -> pretty name
        TCon _ (Tycon _ name _) -> pretty name
        TFun _ a b              -> fromBin "->" a b
        TTuple _ as             -> parens $ hsep $ punctuate comma $ fmap pretty as
        TAp  _ f a            -> fromAp f a
        TGen _ n -> hsep ["gen", pretty n]
        where
          fromAp f a = case f of
            TAp _ (TCon _ (Tycon _ op _)) b
              | env `contains` Op (id'name op) -> fromBin (id'name op) b a
            _ -> fromUn f a

          fromUn f a =  maybeParens (needsParens env ctx OpFunAp) $ hsep
            [ go env (FcLeft OpFunAp) f
            , go env (FcRight OpFunAp) a ]

          fromBin op a b = maybeParens (needsParens env ctx (Op op)) $ hsep
            [ go env (FcLeft $ Op op) a
            , pretty op
            , go env (FcRight $ Op op) b
            ]

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

-------------------------------------------
--

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
initEnv = Map.fromList [(Op "->", OpFix FixRight 2)]

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


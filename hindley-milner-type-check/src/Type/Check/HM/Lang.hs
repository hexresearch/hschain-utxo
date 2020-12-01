{-# Language TypeFamilyDependencies #-}
-- | Main class for the library that defines common types and primitives for the language.
module Type.Check.HM.Lang(
  -- * Lang
    Lang(..)
  , TypeOf
  , TermOf
  , TyTermOf
  , SubstOf
  , ErrorOf
) where

import Type.Check.HM.Term
import Type.Check.HM.Subst
import Type.Check.HM.Type
import Type.Check.HM.TypeError
import Type.Check.HM.TyTerm

-- | Main class to define inference API
class
  ( IsVar (Var q)
  , Show (Src q)
  , Eq (Src q)
  ) => Lang q where
  type Var q = r | r -> q
  type Src q
  type Prim q

  getPrimType :: Prim q -> TypeOf q

type TypeOf q = Type (Src q) (Var q)
type TermOf q = Term (Prim q) (Src q) (Var q)
type TyTermOf q = TyTerm (Prim q) (Src q) (Var q)
type ErrorOf q = TypeError (Src q) (Var q)
type SubstOf q = Subst (Src q) (Var q)



module Type.Loc where

import Type.Type

class HasLoc a where
  getLoc :: a -> Loc

instance HasLoc Id where
  getLoc (Id loc _) = loc

instance HasLoc Kind where
  getLoc = \case
    Star loc -> loc
    Kfun loc _ _ -> loc

instance HasLoc Tyvar where
  getLoc (Tyvar loc _ _) = loc

instance HasLoc Tycon where
  getLoc (Tycon loc _ _) = loc

instance HasLoc Type where
  getLoc = \case
    TVar loc _ -> loc
    TCon loc _ -> loc
    TAp  loc _ _ -> loc
    TFun loc _ _ -> loc
    TTuple loc _ -> loc
    TGen loc _ -> loc

instance HasLoc (Qual t) where
  getLoc (Qual loc _ _) = loc

instance HasLoc Pred where
  getLoc (IsIn loc _ _) = loc

instance HasLoc Scheme where
  getLoc (Forall loc _ _) = loc

instance HasLoc Assump where
  getLoc (idx :>: _) = getLoc idx

instance HasLoc Ambiguity where
  getLoc = getLoc . ambiguity'tyvar


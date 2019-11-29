module Type.Type where

import Data.Maybe
import Data.String
import Data.Text (Text)

type Id = Text

enumId :: Int -> Id
enumId n = "v" <> (fromString $ show n)

newtype TypeError = TypeError Text
  deriving (Semigroup, Monoid)

data Kind
  = Star
  | Kfun Kind Kind
  deriving (Show, Eq, Ord)

data Type
  = TVar Tyvar
  | TCon Tycon
  | TAp Type Type
  | TGen Int
  deriving (Show, Eq, Ord)

data Tyvar = Tyvar Id Kind
  deriving (Show, Eq, Ord)

data Tycon = Tycon Id Kind
  deriving (Show, Eq, Ord)

data Qual t = Qual [Pred] t
  deriving (Show, Eq, Ord)

data Pred = IsIn Id Type
  deriving (Show, Eq, Ord)

data Scheme = Forall [Kind] (Qual Type)
  deriving (Show, Eq, Ord)

data Assump = Id :>: Scheme
  deriving (Show, Eq, Ord)

--------------------------------

class HasKind a where
  kind :: a -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ k) = k

instance HasKind Tycon where
  kind (Tycon _ k) = k

instance HasKind Type where
  kind = \case
    TCon tc -> kind tc
    TVar u  -> kind u
    TAp t _ -> case kind t of
                  Kfun _ k -> k
                  _        -> error "wrong type construction"


instance HasKind (Qual Type) where
  kind (Qual _ t) = kind t

type Ambiguity = (Tyvar, [Pred])


--------------------------------
-- synonyms
--

unitT, charT, intT, integerT, floatT, doubleT,
  listT, arrowT, tuple2T :: Type

unitT    = TCon (Tycon "()" Star)
charT    = TCon (Tycon "Char" Star)
intT     = TCon (Tycon "Int" Star)
integerT = TCon (Tycon "Integer" Star)
floatT   = TCon (Tycon "Float" Star)
doubleT  = TCon (Tycon "Double" Star)

listT    = TCon (Tycon "[]" (Kfun Star Star))
arrowT   = TCon (Tycon "->" (Kfun Star (Kfun Star Star)))
tuple2T  = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

list :: Type -> Type
list a = TAp listT a

fn :: Type -> Type -> Type
fn a b = TAp (TAp arrowT a) b

pair :: Type -> Type -> Type
pair a b = TAp (TAp tuple2T a) b

var :: Text -> Type
var name = TVar (Tyvar name Star)

stringT :: Type
stringT  = list charT

numClasses :: [Id]
numClasses = ["Num", "Integral", "Floating", "Fractional", "Real", "RealFrac"]

stdClasses :: [Id]
stdClasses = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor"
  , "Applicative", "Monad", "MonadPlus", "Semigroup", "Monoid"] ++ numClasses


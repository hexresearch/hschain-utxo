module Type.Type where

import Data.Function (on)
import Data.Maybe
import Data.String
import Data.Text (Text)

import qualified Language.Haskell.Exts.SrcLoc as H

type Loc = H.SrcSpanInfo

noLoc :: Loc
noLoc = H.noSrcSpan

data Id = Id
  { id'loc  :: !Loc
  , id'name :: !Text
  } deriving (Show)

instance IsString Id where
  fromString = Id noLoc . fromString

enumId :: Loc -> Int -> Id
enumId loc n = Id loc $ "v" <> (fromString $ show n)

data TypeError = TypeError
  { typeError'loc     :: [Loc]
  , typeError'message :: [Text]
  }

instance Semigroup TypeError where
  TypeError loc1 msg1 <> TypeError loc2 msg2 = TypeError (loc1 <> loc2) (msg1 <> msg2)

instance Monoid TypeError where
  mempty = TypeError mempty mempty

singleTypeError :: Loc -> Text -> TypeError
singleTypeError loc msg = TypeError [loc] [msg]

data Kind
  = Star Loc
  | Kfun Loc Kind Kind
  deriving (Show)

data Type
  = TVar Loc Tyvar
  | TCon Loc Tycon
  | TFun Loc Type Type
  | TTuple Loc [Type]
  | TAp Loc Type Type
  | TGen Loc Int
  deriving (Show)

data Tyvar = Tyvar Loc Id Kind
  deriving (Show)

data Tycon = Tycon Loc Id Kind
  deriving (Show)

data Qual t = Qual Loc [Pred] t
  deriving (Show, Functor)

data Pred = IsIn Loc Id Type
  deriving (Show)

data Scheme = Forall Loc [Kind] (Qual Type)
  deriving (Show)

data Assump = Id :>: Scheme
  deriving (Show)

--------------------------------

data Ambiguity = Ambiguity
  { ambiguity'tyvar :: Tyvar
  , ambiguity'preds :: [Pred]
  }

--------------------------------

class HasKind a where
  kind :: a -> Kind

instance HasKind Tyvar where
  kind (Tyvar _ _ k) = k

instance HasKind Tycon where
  kind (Tycon _ _ k) = k

instance HasKind Type where
  kind = \case
    TCon _ tc -> kind tc
    TFun loc a b -> Kfun loc (kind a) (kind b)
    TTuple loc _ -> Star loc
    TVar _ u  -> kind u
    TAp _ t _ -> case kind t of
                    Kfun _ _ k -> k
                    _        -> error "wrong type construction"

instance HasKind (Qual Type) where
  kind (Qual _ _ t) = kind t


--------------------------------
-- synonyms
--

unitT, charT, intT, integerT, floatT, doubleT,
  listT :: Type

unitT = unitT' noLoc
charT = charT' noLoc
intT  = intT' noLoc
integerT = integerT' noLoc
floatT = floatT' noLoc
doubleT = doubleT' noLoc

listT = listT' noLoc

unitT', charT', intT', integerT', floatT', doubleT',
  listT' :: Loc -> Type

unitT'    loc = TCon loc (Tycon loc (Id loc "()") (Star loc))
charT'    loc = TCon loc (Tycon loc (Id loc "Char") (Star loc))
intT'     loc = TCon loc (Tycon loc (Id loc "Int") (Star loc))
integerT' loc = TCon loc (Tycon loc (Id loc "Integer") (Star loc))
floatT'   loc = TCon loc (Tycon loc (Id loc "Float") (Star loc))
doubleT'  loc = TCon loc (Tycon loc (Id loc "Double") (Star loc))

listT'    loc = TCon loc (Tycon loc (Id loc "[]") (Kfun loc (Star loc) (Star loc)))

list :: Type -> Type
list = list' noLoc

list' :: Loc -> Type -> Type
list' loc a = TAp loc (listT' loc) a

fn :: Type -> Type -> Type
fn = fn' noLoc

fn' :: Loc -> Type -> Type -> Type
fn' loc a b =  TFun loc a b

pair :: Type -> Type -> Type
pair = pair' noLoc

pair' :: Loc -> Type -> Type -> Type
pair' loc a b = TTuple loc [a, b]

var :: Text -> Type
var = var' noLoc

var' :: Loc -> Text -> Type
var' loc name = TVar loc (Tyvar loc (Id loc name) (Star loc))

stringT :: Type
stringT = stringT' noLoc

stringT' :: Loc -> Type
stringT' loc = list' loc (charT' loc)

numClasses :: [Id]
numClasses = fmap (Id noLoc) ["Num", "Integral", "Floating", "Fractional", "Real", "RealFrac"]

stdClasses :: [Id]
stdClasses = fmap (Id noLoc) ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix", "Functor"
  , "Applicative", "Monad", "MonadPlus", "Semigroup", "Monoid"] ++ numClasses

--------------------------------------------------------------------------
-- instances for equality and ordering comparisions
-- that ignore source location

-- Id

type Id' = Text

uniqueId :: Id -> Id'
uniqueId = id'name

instance Eq Id where
  (==) = (==) `on` uniqueId

instance Ord Id where
  compare = compare `on` uniqueId

-- Kind

data Kind' =
    Star'
  | Kfun' Kind' Kind'
  deriving (Show, Eq, Ord)

uniqueKind :: Kind -> Kind'
uniqueKind = \case
  Star _     -> Star'
  Kfun _ a b -> Kfun' (rec a)  (rec b)
  where
    rec = uniqueKind

instance Eq Kind where
  (==) = (==) `on` uniqueKind

instance Ord Kind where
  compare = compare `on` uniqueKind

-- Type

data Type'
  = TVar' Tyvar'
  | TCon' Tycon'
  | TAp' Type' Type'
  | TFun' Type' Type'
  | TTuple' [Type']
  | TGen' Int
  deriving (Show, Eq, Ord)

uniqueType :: Type -> Type'
uniqueType = \case
  TVar _ v -> TVar' $ uniqueTyvar v
  TCon _ c -> TCon' $ uniqueTycon c
  TAp _ a b -> TAp' (rec a) (rec b)
  TFun _ a b -> TFun' (rec a) (rec b)
  TTuple _ as -> TTuple' (fmap rec as)
  TGen _ n -> TGen' n
  where
    rec = uniqueType

instance Eq Type where
  (==) = (==) `on` uniqueType

instance Ord Type where
  compare = compare `on` uniqueType

-- Tyvar

data Tyvar' = Tyvar' Id' Kind'
  deriving (Show, Eq, Ord)

uniqueTyvar :: Tyvar -> Tyvar'
uniqueTyvar (Tyvar _ id k) = Tyvar' (uniqueId id) (uniqueKind k)

instance Eq Tyvar where
  (==) = (==) `on` uniqueTyvar

instance Ord Tyvar where
  compare = compare `on` uniqueTyvar

-- Tycon

data Tycon' = Tycon' Id' Kind'
  deriving (Show, Eq, Ord)

uniqueTycon :: Tycon -> Tycon'
uniqueTycon (Tycon _ id k) = Tycon' (uniqueId id) (uniqueKind k)

instance Eq Tycon where
  (==) = (==) `on` uniqueTycon

instance Ord Tycon where
  compare = compare `on` uniqueTycon

-- Qual

data Qual' t = Qual' [Pred'] t
  deriving (Show, Eq, Ord)

uniqueQual :: Qual t -> Qual' t
uniqueQual (Qual _ ps t) = Qual' (fmap uniquePred ps) t

instance Eq t => Eq (Qual t) where
  (==) = (==) `on` uniqueQual

instance Ord t => Ord (Qual t) where
  compare = compare `on` uniqueQual

-- Pred

data Pred' = IsIn' Id' Type'
  deriving (Show, Eq, Ord)

uniquePred :: Pred -> Pred'
uniquePred (IsIn _ id ty) = IsIn' (uniqueId id) (uniqueType ty)

instance Eq Pred where
  (==) = (==) `on` uniquePred

instance Ord Pred where
  compare = compare `on` uniquePred

-- Scheme

data Scheme' = Forall' [Kind'] (Qual' Type')
  deriving (Show, Eq, Ord)

uniqueScheme :: Scheme -> Scheme'
uniqueScheme (Forall _ ks q) = Forall' (fmap uniqueKind ks) (uniqueQual $ fmap uniqueType q)

instance Eq Scheme where
  (==) = (==) `on` uniqueScheme

instance Ord Scheme where
  compare = compare `on` uniqueScheme

-- Assump

data Assump' = Assump' Id' Scheme'
  deriving (Show, Eq, Ord)

uniqueAssump :: Assump -> Assump'
uniqueAssump (id :>: sc) = Assump' (uniqueId id) (uniqueScheme sc)

instance Eq Assump where
  (==) = (==) `on` uniqueAssump

instance Ord Assump where
  compare = compare `on` uniqueAssump




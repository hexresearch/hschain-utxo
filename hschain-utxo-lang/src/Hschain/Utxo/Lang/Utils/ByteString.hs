-- | Haskell functions that copy behavior of hschain-utxo primitives
-- for ByteStrings
module Hschain.Utxo.Lang.Utils.ByteString(
    serialiseTerm
  , deserialiseTerm
  , toArgs
  , fromArgs
  , IsTerm(..)
) where

import Control.Applicative
import Codec.Serialise

import Data.ByteString (ByteString)
import Data.Maybe
import Data.Int
import Data.Text (Text)

import Hschain.Utxo.Lang.Core.Compile.Expr (TermVal(..), PrimCon(..))
import Hschain.Utxo.Lang.Core.Types (Prim(..), TypeCore(..))
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types (Args(..))

import qualified Data.ByteString.Lazy as LB
import qualified Data.Vector as V

toArgs :: IsTerm a => a -> Args
toArgs = Args . serialiseTerm

fromArgs :: IsTerm a => Args -> a
fromArgs (Args bs) = deserialiseTerm bs

serialiseTerm :: IsTerm a => a -> ByteString
serialiseTerm = LB.toStrict . serialise . toTerm

deserialiseTerm :: IsTerm a => ByteString -> a
deserialiseTerm = fromJust . fromTerm . deserialise . LB.fromStrict

class IsTerm a where
  termType :: a -> TypeCore
  toTerm   :: a -> TermVal
  fromTerm :: TermVal -> Maybe a

instance IsTerm Int64 where
  termType = const IntT

  toTerm = PrimVal . PrimInt

  fromTerm = \case
    PrimVal (PrimInt n) -> Just n
    _                   -> Nothing

instance IsTerm Int where
  termType = const IntT

  toTerm = PrimVal . PrimInt . fromIntegral

  fromTerm = \case
    PrimVal (PrimInt n) -> Just $ fromIntegral n
    _                   -> Nothing

instance IsTerm Text where
  termType = const TextT

  toTerm = PrimVal . PrimText

  fromTerm = \case
    PrimVal (PrimText n) -> Just n
    _                    -> Nothing

instance IsTerm ByteString where
  termType = const BytesT

  toTerm = PrimVal . PrimBytes

  fromTerm = \case
    PrimVal (PrimBytes n) -> Just n
    _                     -> Nothing

instance IsTerm Bool where
  termType = const BoolT

  toTerm = PrimVal . PrimBool

  fromTerm = \case
    PrimVal (PrimBool b) -> Just b
    _                    -> Nothing

instance IsTerm (Sigma PublicKey) where
  termType = const SigmaT

  toTerm = PrimVal . PrimSigma

  fromTerm = \case
    PrimVal (PrimSigma b) -> Just b
    _                     -> Nothing

instance IsTerm () where
  termType _ = UnitT

  toTerm () = ConVal ConUnit V.empty

  fromTerm = \case
    ConVal ConUnit _ -> Just ()
    _                -> Nothing

instance IsTerm a => IsTerm [a] where
  termType = ListT . termType . listArgType

  toTerm xs = foldr cons nil $ fmap toTerm xs
    where
      ty = termType $ listArgType xs

      nil = ConVal (ConNil ty) V.empty

      cons a as = ConVal (ConCons ty) (V.fromList [a, as])

  fromTerm = \case
    ConVal (ConNil _) _     -> Just []
    ConVal (ConCons _) args -> case V.toList args of
      [a, as] -> liftA2 (:) (fromTerm a) (fromTerm as)
      _       -> Nothing
    _         -> Nothing

listArgType :: [a] -> a
listArgType _ = undefined

instance IsTerm a => IsTerm (Maybe a) where
  termType = MaybeT . termType . maybeArgType

  toTerm xs = maybe nothing just $ fmap toTerm xs
    where
      ty = termType $ maybeArgType xs

      nothing = ConVal (ConNothing ty) V.empty
      just a  = ConVal (ConJust ty) (V.singleton a)

  fromTerm = \case
    ConVal (ConNothing _) _ -> Just Nothing
    ConVal (ConJust _) args -> case V.toList args of
      [a] -> fmap Just (fromTerm a)
      _   -> Nothing
    _         -> Nothing

maybeArgType :: Maybe a -> a
maybeArgType _ = undefined

instance (IsTerm a, IsTerm b) => IsTerm (a, b) where
  termType a = TupleT $ tupleTypeList2 a

  toTerm tup@(a, b) =
    ConVal (ConTuple (V.fromList $ tupleTypeList2 tup)) $ V.fromList [toTerm a, toTerm b]

  fromTerm = \case
    ConVal (ConTuple _) args -> case V.toList args of
      [a, b] -> liftA2 (,) (fromTerm a) (fromTerm b)
      _      -> Nothing
    _                        -> Nothing

tupleTypeList2 :: (IsTerm a, IsTerm b) => (a, b) -> [TypeCore]
tupleTypeList2 a = [termType $ tupleTupe2_1 a, termType $ tupleTupe2_2 a]
  where
    tupleTupe2_1 :: (a, b) -> a
    tupleTupe2_1 _ = undefined

    tupleTupe2_2 :: (a, b) -> b
    tupleTupe2_2 _ = undefined

instance (IsTerm a, IsTerm b, IsTerm c) => IsTerm (a, b, c) where
  termType a = TupleT $ tupleTypeList3 a

  toTerm tup@(a, b, c) =
    ConVal (ConTuple (V.fromList $ tupleTypeList3 tup)) $ V.fromList [toTerm a, toTerm b, toTerm c]

  fromTerm = \case
    ConVal (ConTuple _) args -> case V.toList args of
      [a, b, c] -> liftA3 (,,) (fromTerm a) (fromTerm b) (fromTerm c)
      _        -> Nothing
    _                        -> Nothing

tupleTypeList3 :: (IsTerm a, IsTerm b, IsTerm c) => (a, b, c) -> [TypeCore]
tupleTypeList3 a = [termType $ tupleTupe3_1 a, termType $ tupleTupe3_2 a, termType $ tupleTupe3_3 a]
  where
    tupleTupe3_1 :: (a, b, c) -> a
    tupleTupe3_1 _ = undefined

    tupleTupe3_2 :: (a, b, c) -> b
    tupleTupe3_2 _ = undefined

    tupleTupe3_3 :: (a, b, c) -> c
    tupleTupe3_3 _ = undefined



module Hschain.Utxo.Lang.Sigma.Expr where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

import qualified Codec.Serialise as CBOR
import qualified Codec.Serialise.Decoding as CBOR
import Data.Bits
import Data.Foldable
import Data.Function (on)
import Data.List (find)
import Data.Maybe
import Data.Monoid (All(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import qualified Data.ByteString         as BS
import qualified Data.ByteString.Lazy    as BL
import qualified Data.ByteArray          as BA
import Data.Coerce
import qualified Crypto.ECC.Edwards25519 as Ed
import qualified Crypto.Hash.Algorithms  as Hash
import qualified Crypto.Hash             as Hash
import qualified Crypto.Random.Types     as RND
import Crypto.Error
import GHC.Generics (Generic)

import qualified Data.Sequence as Seq

-- import Debug.Trace
-- import Text.Show.Pretty

----------------------------------------------------------------
-- Operations with elliptic curves
----------------------------------------------------------------

newtype Prove a = Prove (ExceptT Text IO a)
  deriving (Functor, Monad, Applicative, MonadError Text, MonadIO)

runProve :: Prove a -> IO (Either Text a)
runProve (Prove p) = runExceptT p

-- | Operations with elliptic curve
class EC a where
  data ECPoint   a
  data ECScalar  a
  data Challenge a
  -- Challenge part
  generateChallenge :: IO (Challenge a)
  randomOracle      :: BS.ByteString -> Challenge a
  xorChallenge      :: Challenge a -> Challenge a -> Challenge a

  generateScalar    :: IO (ECScalar a)
  fromGenerator     :: ECScalar  a -> ECPoint  a
  fromChallenge     :: Challenge a -> ECScalar a
  (.+.)   :: ECScalar a -> ECScalar a -> ECScalar a
  (.*.)   :: ECScalar a -> ECScalar a -> ECScalar a

  (.*^)   :: ECScalar a -> ECPoint  a -> ECPoint  a
  -- Group operations
  (^+^)   :: ECPoint  a -> ECPoint  a -> ECPoint  a
  negateP :: ECPoint a -> ECPoint a

type Commitment a = ECPoint a
type Response a   = ECScalar a

data Ed25519

instance EC Ed25519 where
  newtype ECPoint   Ed25519 = ECPoint25519  Ed.Point
  newtype ECScalar  Ed25519 = ECScalar25519 Ed.Scalar
  newtype Challenge Ed25519 = ChallengeEd25519 BS.ByteString
  generateChallenge = ChallengeEd25519 <$> RND.getRandomBytes 31
  randomOracle
    = ChallengeEd25519
    . BS.take 31
    . BA.convert
    . Hash.hash @_ @Hash.SHA256
  xorChallenge (ChallengeEd25519 a) (ChallengeEd25519 b)
    = ChallengeEd25519
    $ BS.pack
    $ BS.zipWith xor a b

  generateScalar    = coerce (Ed.scalarGenerate @IO)
  fromGenerator     = coerce Ed.toPoint
  -- FIXME: We need to maintain that challenge is less than group
  --        module, right?
  fromChallenge (ChallengeEd25519 bs) =
    case Ed.scalarDecodeLong $ BS.take 31 bs of
      CryptoPassed x -> ECScalar25519 x
      CryptoFailed e -> error (show e)
  (.+.)   = coerce Ed.scalarAdd
  (.*.)   = coerce Ed.scalarMul
  (^+^)   = coerce Ed.pointAdd
  (.*^)   = coerce Ed.pointMul
  negateP = coerce Ed.pointNegate

instance Ord Ed.Point where
  compare = compare `on` (Ed.pointEncode :: Ed.Point -> BS.ByteString)

instance Ord Ed.Scalar where
  compare = compare `on` (Ed.scalarEncode :: Ed.Scalar -> BS.ByteString)

deriving instance Show (ECPoint   Ed25519)
deriving instance Show (ECScalar  Ed25519)
deriving instance Show (Challenge Ed25519)
deriving instance Show (Secret    Ed25519)
deriving instance Eq   (ECPoint   Ed25519)
deriving instance Eq   (ECScalar  Ed25519)
deriving instance Eq   (Challenge Ed25519)
deriving instance Eq   (Secret    Ed25519)
deriving instance Ord  (ECPoint   Ed25519)
deriving instance Ord  (ECScalar  Ed25519)
deriving instance Ord  (Challenge Ed25519)
deriving instance Ord  (Secret    Ed25519)



instance CBOR.Serialise (ECPoint Ed25519) where
  encode = CBOR.encode . id @BS.ByteString . Ed.pointEncode . coerce
  decode = decodeBy (fmap coerce . Ed.pointDecode) =<< CBOR.decode

instance CBOR.Serialise (ECScalar Ed25519) where
  encode = CBOR.encode . id @BS.ByteString . Ed.scalarEncode . coerce
  decode = decodeBy (fmap coerce . Ed.scalarDecodeLong) =<< CBOR.decode

instance CBOR.Serialise (Challenge Ed25519) where
  encode (ChallengeEd25519 bs) = CBOR.encode bs
  decode = fmap ChallengeEd25519 CBOR.decode

decodeBy :: (BS.ByteString -> CryptoFailable a) -> BS.ByteString -> CBOR.Decoder s a
decodeBy decoder bs = case decoder bs of
  CryptoPassed a   -> return a
  CryptoFailed err -> fail $ show err

fiatShamirCommitment :: (EC a, CBOR.Serialise b) => b -> Challenge a
fiatShamirCommitment = randomOracle . BL.toStrict . CBOR.serialise

----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

newtype Secret    a = Secret    { unSecret    :: ECScalar a
  } deriving (Generic)

newtype PublicKey a = PublicKey { unPublicKey :: ECPoint  a
  } deriving (Generic)

data KeyPair a = KeyPair
  { secretKey :: Secret a
  , publicKey :: PublicKey a
  }

deriving stock   instance Show (ECPoint a) => Show (PublicKey a)
deriving stock   instance Eq   (ECPoint a) => Eq   (PublicKey a)
deriving stock   instance Ord  (ECPoint a) => Ord  (PublicKey a)
deriving newtype instance (CBOR.Serialise (ECPoint a)) => CBOR.Serialise (PublicKey a)

generateSecretKey :: EC a => IO (Secret a)
generateSecretKey = coerce generateScalar

getPublicKey :: EC a => Secret a -> PublicKey a
getPublicKey = coerce fromGenerator

generateKeyPair :: EC a => IO (KeyPair a)
generateKeyPair = do
  s <- generateSecretKey
  return $ KeyPair s (getPublicKey s)


----------------------------------------------------------------
-- Σ-expression
----------------------------------------------------------------

-- | Expression that should be proven
data SigmaE k a
  = Leaf k a
    -- ^ Proof of possession of discrete logarithm of point at
    --   elliptic curve
  | AND k [SigmaE k a]
    -- ^ AND connective
  | OR  k [SigmaE k a]
  deriving (Functor, Foldable, Traversable, Show)

sexprAnn :: SigmaE k a -> k
sexprAnn = \case
  Leaf k _ -> k
  AND     k _ -> k
  OR      k _ -> k

-- | Proof of sigma expression
data SigmaProof a
  = SProofDL  !(ProofDL a)
  | SProofAND
  | SProofOR

-- | Set of known keys
newtype Env a = Env [KeyPair a]

----------------------------------------------------------------
-- Intermediate data types
----------------------------------------------------------------

-- Whether we create real proof or simulate it
data ProofVar
  = Real
  | Simulated
  deriving (Show,Eq)

newProof :: (EC a, Eq (ECPoint a), CBOR.Serialise (ECPoint a))
  => Env a -> SigmaE () (PublicKey a) -> IO (Either Text (Proof a))
newProof env expr = runProve $ do
  commitments <- generateCommitments (markTree env expr)
  toProof =<< generateProofs env commitments

toProof :: SigmaE (ProofTag a) (ProofDL a) -> Prove (Proof a)
toProof tree = Prove $ ExceptT $ pure $ liftA2 Proof (getRootChallenge tree) (getProvenTree tree)
  where
    getRootChallenge =
      maybe (Left "No root challenge") Right . proofTag'challenge . sexprAnn

    getProvenTree x = case x of
      Leaf tag p -> Right $ ProvenLeaf (responseZ p) (publicK p)
      AND _ es   -> ProvenAnd  <$> traverse getProvenTree es
      OR  _ es   -> case es of
        []   -> Left "No children for OR-node"
        e:es -> liftA2 ProvenOr (getOrleftmostChild e) (getOrRest es)
      where
        getOrleftmostChild x = getProvenTree x

        getOrRest xs = fmap Seq.fromList $ traverse go xs
          where
            go x = do
              ch <- maybe (Left err) Right $ proofTag'challenge $ sexprAnn x
              t  <- getProvenTree x
              return $ OrChild ch t

            err = "No challenge for OR child node"


data ProofTag a = ProofTag
  { proofTag'flag      :: ProofVar
  , proofTag'challenge :: Maybe (Challenge a)
  }

deriving stock   instance Show (Challenge a) => Show (ProofTag a)

-- Mark all nodes according to whether we can produce proof for them
markTree :: (EC a, Eq (ECPoint a)) => Env a -> SigmaE () (PublicKey a) -> SigmaE ProofVar (PublicKey a)
markTree (Env env) = clean . check
  where
    -- Select nodes for which we could provide proof. We may mark more nodes that necessary
    check = \case
      Leaf () k  -> Leaf (if k `elem` knownPK then Real else Simulated) k
      AND  () es -> AND k es'
        where
          es'  = map check es
          k | all ((==Real) . sexprAnn) es' = Real
            | otherwise                     = Simulated
      OR   () es -> OR k es'
        where
          es'  = map check es
          k | any ((==Real) . sexprAnn) es' = Real
            | otherwise                     = Simulated
    knownPK = publicKey <$> env
    -- Reduce proof so that OR node has only necessary number of real children
    clean expr = case expr of
      Leaf{}           -> expr
      AND Simulated es -> AND Simulated $ markSim <$> es
      AND Real      es -> AND Real      $ clean   <$> es
      OR  Simulated es -> OR  Simulated $ markSim <$> es
      OR  Real      es -> OR  Real      $ splitOR es
    -- Mark all nodes as simulated
    markSim = \case
      Leaf _ a  -> Leaf Simulated   a
      AND  _ es -> AND  Simulated $ markSim <$> es
      OR   _ es -> OR   Simulated $ markSim <$> es
    -- Only leave one leaf of OR as real
    splitOR []     = error "Impossible"
    splitOR (e:es) = case sexprAnn e of
      Simulated -> markSim e : splitOR es
      Real      -> clean   e : fmap markSim es

-- Genererate simalated proofs and commitments for real proofs
generateCommitments
  :: (EC a)
  => SigmaE ProofVar (PublicKey a)
  -> Prove (SigmaE (ProofTag a) (Either (PartialProof a) (ProofDL a)))
generateCommitments tree = case sexprAnn tree of
  Real      -> goReal tree
  Simulated -> throwError "The root is simulated. Can not produce the proof."
  where
    -- Go down expecting real node
    goReal = \case
      Leaf Real k -> do r <- liftIO generateScalar
                        return $ Leaf (ProofTag Real Nothing) $ Left $ PartialProof
                          { pproofPK = k
                          , pproofR  = r
                          , pproofA  = fromGenerator r
                          }
      AND Real es -> AND (ProofTag Real Nothing) <$> traverse goReal     es
      OR  Real es -> OR  (ProofTag Real Nothing) <$> traverse simulateOR es
        where
          simulateOR e = case sexprAnn e of
            Real      -> goReal e
            Simulated -> do ch <- liftIO generateChallenge
                            goSim ch e
      _ -> throwError "Simulated node!"
    --
    goSim ch = \case
      Leaf Simulated k      -> liftIO $ Leaf (ProofTag Simulated $ Just ch) . Right <$> simulateProofDL k ch
      AND  Simulated es     -> AND  (ProofTag Simulated $ Just ch) <$> traverse (goSim ch) es
      OR   Simulated []     -> throwError "Empty OR"
      OR   Simulated (e:es) -> do esWithCh <- liftIO $ forM es $ \x -> (,x) <$> generateChallenge
                                  let ch0 = foldl xorChallenge ch $ map fst esWithCh
                                  OR (ProofTag Simulated $ Just ch) <$> traverse (uncurry goSim) ((ch0,e) : esWithCh)
      _ -> throwError "Real node"

toFiatShamir
  :: SigmaE k (FiatShamirLeaf a)
  -> FiatShamir a
toFiatShamir = \case
  Leaf _ leaf -> FSDLog leaf
  AND  _ es   -> FSAnd (toFiatShamir <$> es)
  OR   _ es   -> FSOr  (toFiatShamir <$> es)


generateProofs
  :: forall a. (EC a, Eq (ECPoint a), CBOR.Serialise (ECPoint a))
  => Env a
  -> SigmaE (ProofTag a) (Either (PartialProof a) (ProofDL a))
  -> Prove (SigmaE (ProofTag a) (ProofDL a))
generateProofs (Env env) expr0 = goReal ch0 expr0
  where
    withChallenge ch tag = tag { proofTag'challenge = Just ch }

    ch0 :: Challenge a
    ch0 = fiatShamirCommitment $ toFiatShamir $ fmap extractCommitment expr0

    extractCommitment :: Either (PartialProof a) (ProofDL a) -> FiatShamirLeaf a
    extractCommitment =
      either
        (\x -> FiatShamirLeaf (pproofPK x) (pproofA x))
        (\x -> FiatShamirLeaf (publicK x)  (commitmentA x))
    --
    goReal ch = \case
      Leaf tag eProof -> case (proofTag'flag tag, eProof) of
        (Real, (Left PartialProof{..})) -> do
          let e = fromChallenge ch
              [Secret sk] = [ secretKey | KeyPair{..} <- env
                                        , pproofPK == publicKey
                                        ]
              z = pproofR .+. (sk .*. e)
          return $ Leaf (ProofTag Real $ Just ch) $ ProofDL { publicK     = pproofPK
                                    , commitmentA = pproofA
                                    , challengeE  = ch
                                    , responseZ   = z
                                    }
        (Simulated, Right e)   -> return $ Leaf (ProofTag Simulated $ Just ch) e
      AND tag es -> case proofTag'flag tag of
        Real      -> AND (withChallenge ch tag) <$> traverse (goReal ch) es
        Simulated -> AND (withChallenge ch tag) <$> traverse goSim es
      OR tag es  -> case proofTag'flag tag of
        Real      -> OR (withChallenge ch tag) <$> orChildren ch es
        Simulated -> OR (withChallenge ch tag) <$> traverse goSim es
      where
        orChildren rootCh children = do
          let challenges = catMaybes $ fmap (proofTag'challenge . sexprAnn) children
              challengeForReal = orChallenge rootCh challenges
          traverse (\x -> if Real == (proofTag'flag $ sexprAnn x)
            then goReal challengeForReal x
            else goSim x
            ) children

    -- This step is not needed, but might be useful to prevent side-channel timing attacks.
    goSim = \case
      Leaf tag eProof -> case (proofTag'flag tag, eProof) of
        (Simulated, Right pdl)  -> return $ Leaf tag pdl
        _                       -> err
      AND tag es     -> case proofTag'flag tag of
        Simulated  -> AND tag <$> traverse goSim es
        _          -> err
      OR tag es      -> case proofTag'flag tag of
        Simulated  -> OR tag <$> traverse goSim es
        _          -> err
      where
        err = throwError "Simulated parent node has real children"

-- Partial proof of possession of discrete logarithm
data PartialProof a = PartialProof
  { pproofPK :: PublicKey a
  , pproofR  :: ECScalar  a
  , pproofA  :: ECPoint   a
  }
deriving instance ( Show (ECPoint   a)
                  , Show (Secret    a)
                  , Show (ECScalar a)
                  , Show (Challenge a)
                  ) => Show (PartialProof a)

-- | Proof of knowledge of discrete logarithm
data ProofDL a = ProofDL
  { publicK     :: PublicKey a
  , commitmentA :: Commitment a
  , responseZ   :: Response a
  , challengeE  :: Challenge a
  } deriving (Generic)

deriving instance ( Show (ECPoint   a)
                  , Show (ECScalar  a)
                  , Show (Challenge a)
                  ) => Show (ProofDL a)

deriving instance ( Eq (ECPoint   a)
                  , Eq (ECScalar  a)
                  , Eq (Challenge a)
                  ) => Eq (ProofDL a)

instance ( CBOR.Serialise (ECPoint   a)
         , CBOR.Serialise (ECScalar  a)
         , CBOR.Serialise (Challenge a)
         ) => CBOR.Serialise (ProofDL a)

data SimTree a
  = SimDL  (Either (ProofDL a) (PublicKey a))
  | SimOR  [SimTree a] (SimTree a) [SimTree a]
  | SimAND [SimTree a]

-- | Tree that is used as input to Fiat-Shamir hash function
data FiatShamir a
  = FSDLog (FiatShamirLeaf a)
  | FSAnd [FiatShamir a]
  | FSOr  [FiatShamir a]
  deriving (Generic)

data FiatShamirLeaf a = FiatShamirLeaf
  { fsLeaf'publicKey  :: PublicKey a
  , fsLeaf'commitment :: Commitment a
  } deriving (Generic)

deriving instance ( Show (PublicKey   a)
                  , Show (Commitment  a)
                  ) => Show (FiatShamir a)

deriving instance ( Show (PublicKey   a)
                  , Show (Commitment  a)
                  ) => Show (FiatShamirLeaf a)

instance ( CBOR.Serialise (ECPoint a)
         ) => CBOR.Serialise (FiatShamir a)

instance ( CBOR.Serialise (ECPoint a)
         ) => CBOR.Serialise (FiatShamirLeaf a)

data Proof a = Proof
  { proof'rootChallenge :: Challenge a
  , proof'tree          :: ProvenTree a
  } deriving (Generic)

instance ( CBOR.Serialise (ECPoint a), CBOR.Serialise (ECScalar a), CBOR.Serialise (Challenge a)
         ) => CBOR.Serialise (Proof a)

deriving stock   instance (Show (ECPoint a), Show (ECScalar a), Show (Challenge a)) => Show (Proof a)
deriving stock   instance (Eq   (ECPoint a), Eq   (ECScalar a), Eq   (Challenge a)) => Eq   (Proof a)

data ProvenTree a
  = ProvenLeaf
      { provenLeaf'responceZ :: ECScalar a
      , provenLeaf'publicK   :: PublicKey a
      }
  | ProvenOr
      { provenOr'leftmost  :: ProvenTree a
      , provenOr'rest      :: Seq (OrChild a)
      }
  | ProvenAnd
      { provenAnd'children :: [ProvenTree a]
      }
  deriving (Generic)

instance ( CBOR.Serialise (ECPoint a), CBOR.Serialise (ECScalar a), CBOR.Serialise (Challenge a)
         ) => CBOR.Serialise (ProvenTree a)

deriving stock   instance (Show (ECPoint a), Show (ECScalar a), Show (Challenge a)) => Show (ProvenTree a)
deriving stock   instance (Eq   (ECPoint a), Eq   (ECScalar a), Eq   (Challenge a)) => Eq   (ProvenTree a)
-- deriving stock   instance Show (ECPoint a) => Show (Proof a)

data OrChild a = OrChild
  { orChild'challenge :: Challenge a
  , orChild'tree      :: ProvenTree a
  } deriving (Generic)


instance ( CBOR.Serialise (ECPoint a), CBOR.Serialise (ECScalar a), CBOR.Serialise (Challenge a)
         ) => CBOR.Serialise (OrChild a)

deriving stock   instance (Show (ECPoint a), Show (ECScalar a), Show (Challenge a)) => Show (OrChild a)
deriving stock   instance (Eq   (ECPoint a), Eq   (ECScalar a), Eq   (Challenge a)) => Eq   (OrChild a)

completeProvenTree :: EC a => Proof a -> SigmaE () (ProofDL a)
completeProvenTree Proof{..} = go proof'rootChallenge proof'tree
  where
    go ch tree = case tree of
      ProvenLeaf resp pubKey  -> Leaf () $ getProofDL ch pubKey resp
      ProvenOr leftmost rest -> OR   () $ toList $ fmap (\OrChild{..} -> go orChild'challenge orChild'tree) $
                                              (getLeftmostOrChallenge ch leftmost rest) Seq.<| rest
      ProvenAnd children      -> AND  () $ fmap (go ch) children

    getProofDL ch pubKey respZ = ProofDL
        { publicK     = pubKey
        , commitmentA = getCommitment respZ ch pubKey
        , responseZ   = respZ
        , challengeE  = ch
        }

    getLeftmostOrChallenge ch leftmost children = OrChild
      { orChild'challenge = orChallenge ch (toList $ fmap orChild'challenge children)
      , orChild'tree      = leftmost
      }

orChallenge :: EC a => Challenge a -> [Challenge a] -> Challenge a
orChallenge ch rest = foldl xorChallenge ch rest

verifyProof :: forall a. (EC a, Eq (ECPoint a), CBOR.Serialise (ECPoint a), Eq (Challenge a))
  => Proof a -> Bool
verifyProof proof =
     checkProofs compTree
  && (checkHash (getHash compTree))
  where
    checkProofs :: SigmaE b (ProofDL a) -> Bool
    checkProofs = getAll . foldMap (All . verifyProofDL)

    checkHash hash = proof'rootChallenge proof == hash

    getHash tree = fiatShamirCommitment $ toFiatShamir $ fmap extractFiatShamirLeaf tree
      where
        extractFiatShamirLeaf ProofDL{..} = FiatShamirLeaf publicK commitmentA

    compTree = completeProvenTree proof

----------------------------------------------------------------
-- Primitives for Σ-expressions
----------------------------------------------------------------

-- Simulate proof of posession of discrete logarithm for given
-- challenge
simulateProofDL :: EC a => PublicKey a -> Challenge a -> IO (ProofDL a)
simulateProofDL pk e = do
  z <- generateScalar
  return ProofDL
    { publicK     = pk
    , commitmentA =  getCommitment z e pk
    , responseZ   = z
    , challengeE  = e
    }

getCommitment :: EC a => Response a -> Challenge a -> PublicKey a -> Commitment a
getCommitment z ch pk = fromGenerator z ^+^ negateP (fromChallenge ch .*^ unPublicKey pk)

verifyProofDL :: (EC a, Eq (ECPoint a)) => ProofDL a -> Bool
verifyProofDL ProofDL{..}
  = fromGenerator responseZ == (commitmentA ^+^ (fromChallenge challengeE .*^ unPublicKey publicK))

{-
go = do
  kp1 :: KeyPair Ed25519 <- generateKeyPair
  kp2 :: KeyPair Ed25519 <- generateKeyPair
  kp3 :: KeyPair Ed25519 <- generateKeyPair
  kp4 :: KeyPair Ed25519 <- generateKeyPair
  -- Expression to prove
  let leaf = Leaf () . publicKey
  let expr = OR () [ leaf kp1
                    , leaf kp2
                    ]
      env  = Env [kp1,kp2,kp3]
  -- Generating proof
  let marked = markTree env expr
  commited <- generateCommitments marked
  proof    <- generateProofs env commited
  --
  putStrLn $ groom expr
  putStrLn ""
  putStrLn $ groom marked
  putStrLn ""
  putStrLn $ groom commited
  putStrLn ""
  putStrLn $ groom proof
  putStrLn ""
  putStrLn $ groom $ verifyProof proof
-}

{- For debug

traceMsg :: Show a => String -> a -> a
traceMsg msg a = trace (mconcat [msg, ": ", ppShow a]) a
-}


-- | The module defines functions to create proofs for sigma-expressions
-- and verify them.
--
-- Implemented by the paper: "ErgoScript, a Cryptocurrency Scripting Language
-- Supporting Noninteractive Zero-Knowledge Proofs" by Ergo Developers
module Hschain.Utxo.Lang.Sigma.Interpreter(
    Proof
  , newProof
  , verifyProof
  , ProvenTree(..)
  , OrChild(..)
  , completeProvenTree
  , ProofTag(..)
  , ProofVar(..)
  , ProofSim(..)
  , Prove(..)
  , runProve
  , initRootChallenge
  , orChallenge
  , toProof
  , markTree
  , ownsKey
  , getResponseForInput
  -- * New API
  , simulateProofs
  , makeLocalCommitements
  , computeRealChallenges
  , evaluateRealProof
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

import qualified Codec.Serialise as CBOR
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.ByteString.Lazy as LB
import qualified Data.Aeson           as JSON
import qualified Data.Set             as Set
import GHC.Generics (Generic)

import HSChain.Crypto.Classes.Hash (genericHashStep)
import HSChain.Crypto              (CryptoHashable(..), PublicKey, PrivKey, CryptoAsymmetric(..))
import Hschain.Utxo.Lang.Sigma.DLog
import Hschain.Utxo.Lang.Sigma.DTuple
import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.FiatShamirTree
import Hschain.Utxo.Lang.Sigma.Protocol
import Hschain.Utxo.Lang.Sigma.Types


----------------------------------------------------------------
-- Data types
----------------------------------------------------------------

-- | Prove monad
newtype Prove a = Prove (ExceptT Text IO a)
  deriving newtype (Functor, Monad, Applicative, MonadError Text, MonadIO)

-- | Run prove monad.
runProve :: Prove a -> IO (Either Text a)
runProve (Prove p) = runExceptT p

----------------------------------------------------

-- | Annotation which shows whether node is real or we simulate it.
data ProofVar
  = Real          -- ^ real proof tag
  | Simulated     -- ^ simulated proof tag
  deriving (Show,Eq)

-- | Annotation for point when we added challenges for every simulated
--   node
data ProofSim a
  = RealS
  | SimulatedS (Challenge a)

-- | Proof tag and challenge.
data ProofTag a = ProofTag
  { proofTag'flag      :: ProofVar
  , proofTag'challenge :: Maybe (Challenge a)
  }

deriving stock   instance Eq   (Challenge a) => Eq   (ProofTag a)
deriving stock   instance Show (Challenge a) => Show (ProofTag a)

-- Partial proof of possession of discrete logarithm
data PartialProof a = PartialProof
  { pproofInput :: ProofInput a
  , pproofR     :: ECScalar   a
  }

deriving instance ( Show (ECPoint   a)
                  , Show (ECScalar a)
                  , Show (Challenge a)
                  ) => Show (PartialProof a)

-- | Proof to reconstruct all challenges from the root challenge.
data Proof a = Proof
  { proof'rootChallenge :: Challenge a   -- ^ root chalenge
  , proof'tree          :: ProvenTree a  -- ^ expression to prove
  } deriving (Generic)

instance (EC a) => CBOR.Serialise (Proof a)
instance (EC a) => JSON.FromJSON  (Proof a)
instance (EC a) => JSON.ToJSON    (Proof a)

instance ( CryptoHashable (ECPoint a)
         , CryptoHashable (ECScalar a)
         , CryptoHashable (Challenge a)
         ) => CryptoHashable (Proof a) where
  hashStep = genericHashStep hashDomain

deriving stock   instance (Show (ECPoint a), Show (ECScalar a), Show (Challenge a)) => Show (Proof a)
deriving stock   instance (Eq   (ECPoint a), Eq   (ECScalar a), Eq   (Challenge a)) => Eq   (Proof a)
deriving stock   instance (Ord  (ECPoint a), Ord  (ECScalar a), Ord  (Challenge a)) => Ord  (Proof a)

deriving anyclass instance (NFData (ECPoint a), NFData (ECScalar a), NFData (Challenge a)) => NFData (Proof a)


-- | Expression to prove.
data ProvenTree a
  = ProvenLeaf
      { provenLeaf'responseZ :: ECScalar a
      , provenLeaf'publicK   :: ProofInput a
      }
  | ProvenOr
      { provenOr'leftmost  :: ProvenTree a
      , provenOr'rest      :: [OrChild a]
      } -- ^ we keep chalenges for all children but for the leftmost one.
  | ProvenAnd
      { provenAnd'children :: [ProvenTree a]
      } -- ^ chalenges are calculated
  deriving (Generic)

instance (EC a) => CBOR.Serialise (ProvenTree a)
instance (EC a) => JSON.FromJSON  (ProvenTree a)
instance (EC a) => JSON.ToJSON    (ProvenTree a)

instance ( CryptoHashable (ECScalar  a)
         , CryptoHashable (ECPoint   a)
         , CryptoHashable (Challenge a)
         ) => CryptoHashable (ProvenTree a) where
  hashStep = genericHashStep hashDomain

deriving stock   instance (Show (ECPoint a), Show (ECScalar a), Show (Challenge a)) => Show (ProvenTree a)
deriving stock   instance (Eq   (ECPoint a), Eq   (ECScalar a), Eq   (Challenge a)) => Eq   (ProvenTree a)
deriving stock   instance (Ord  (ECPoint a), Ord  (ECScalar a), Ord  (Challenge a)) => Ord  (ProvenTree a)
deriving anyclass instance (NFData (ECPoint a), NFData (ECScalar a), NFData (Challenge a)) => NFData (ProvenTree a)

-- | Or-child should contain expression and challenge.
data OrChild a = OrChild
  { orChild'challenge :: Challenge a
  , orChild'tree      :: ProvenTree a
  } deriving (Generic)

instance (EC a) => CBOR.Serialise (OrChild a)
instance (EC a) => JSON.FromJSON  (OrChild a)
instance (EC a) => JSON.ToJSON    (OrChild a)

instance ( CryptoHashable (ECScalar  a)
         , CryptoHashable (ECPoint   a)
         , CryptoHashable (Challenge a)
         ) => CryptoHashable (OrChild a) where
  hashStep = genericHashStep hashDomain


deriving stock    instance (Show   (ECPoint a), Show   (ECScalar a), Show   (Challenge a)) => Show   (OrChild a)
deriving stock    instance (Eq     (ECPoint a), Eq     (ECScalar a), Eq     (Challenge a)) => Eq     (OrChild a)
deriving stock    instance (Ord    (ECPoint a), Ord    (ECScalar a), Ord    (Challenge a)) => Ord    (OrChild a)
deriving anyclass instance (NFData (ECPoint a), NFData (ECScalar a), NFData (Challenge a)) => NFData (OrChild a)


----------------------------------------------------------------
-- Creation of proofs
----------------------------------------------------------------


-- | Create proof for sigma expression based on ownership of collection of keys (@Env@)
newProof :: (EC a)
  => Env a -> SigmaE () (ProofInput a) -> ByteString -> IO (Either Text (Proof a))
newProof env expr message = runProve $ do
  commitments <- generateCommitments (markTree isProvable expr)
  toProof =<< generateProofs env commitments message
  where
    isProvable input = leafPublicKey input `Set.member` knownKeys
    knownKeys = Set.fromList $ getPublicKey <$> unEnv env

-- Syntactic step that performs a type conversion only
toProof :: SigmaE (Challenge a) (AtomicProof a) -> Prove (Proof a)
toProof tree = Proof (sexprAnn tree) <$> getProvenTree tree
  where
    getProvenTree ptree = case ptree of
      Leaf _ p  -> pure $ ProvenLeaf (responseZ p) (getProofInput p)
      AND  _ es -> ProvenAnd <$> traverse getProvenTree es
      OR   _ es -> case es of
        []   -> error "toProof: No children for OR-node"
        a:as -> liftA2 ProvenOr (getOrleftmostChild a) (getOrRest as)
      where
        getOrleftmostChild = getProvenTree
        getOrRest = traverse go
          where
            go x = OrChild (sexprAnn x) <$> getProvenTree x


ownsKey :: EC a => Set (PublicKey a) -> ProofInput a -> Bool
ownsKey knownPKs = checkKey . leafPublicKey
  where
    checkKey k = k `Set.member` knownPKs

----------------------------------------------------------------
-- Building proof
----------------------------------------------------------------

-- | First stage of building proof of sigma expression. We need to
--   decide which nodes will get real proof and which are simulated.
markTree
  :: (a -> Bool)        -- ^ Predicate that shows whether we can prove leaf
  -> SigmaE ()       a  -- ^ Original sigma expression
  -> SigmaE ProofVar a  -- ^ Sigma expression with leafs marked
markTree isProvable = clean . check
  where
    -- Step 1: Mark as real everything the prover can prove.
    check = \case
      Leaf () leaf | isProvable leaf -> Leaf Real      leaf
                   | otherwise       -> Leaf Simulated leaf
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
    -- Change some "real" nodes to "simulated" to make sure each node
    -- has the right number of simulated children.
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


-- | We generate challenges for every simulated node in the expression
--   tree and simulate proof for them.
--
--   Root node must be marked as 'Real'
simulateProofs
  :: (EC a, MonadIO m)
  => SigmaE ProofVar    (ProofInput a)
  -> m (SigmaE (ProofSim a) (Either (ProofInput a) (AtomicProof a)))
simulateProofs = goReal
  where
    goReal = \case
      Leaf Real leaf -> pure $ Leaf RealS (Left leaf)
      AND  Real es   -> AND RealS <$> traverse goReal es
      OR   Real es   -> OR  RealS <$> traverse simulateOR es
        where
          simulateOR e = case sexprAnn e of
            Real      -> goReal e
            Simulated -> do ch <- liftIO generateChallenge
                            goSim ch e
      _ -> error "simulateProofs: simulated proof encountered"
    goSim ch = \case
      Leaf Simulated leaf   -> liftIO $ Leaf (SimulatedS ch) . Right <$> simulateAtomicProof leaf ch
      AND  Simulated es     -> AND (SimulatedS ch) <$> traverse (goSim ch) es
      OR   Simulated []     -> error "simulateProofs: Empty OR"
      OR   Simulated (e:es) -> do
        esWithCh <- liftIO $ forM es $ \x -> (,x) <$> generateChallenge
        let ch0 = foldl xorChallenge ch $ map fst esWithCh
        OR (SimulatedS ch) <$> traverse (uncurry goSim) ((ch0,e) : esWithCh)
      _ -> error "simulateProofs: internal error"

-- | Make commitments for all real leaves
makeLocalCommitements
  :: (EC a, MonadIO m)
  => SigmaE t (Either (ProofInput a) (AtomicProof a))
  -> m (SigmaE t (Either (PartialProof a) (AtomicProof a)))
makeLocalCommitements = traverse commit
  where
    commit (Right p) = pure $ Right p
    commit (Left  p) = go p
    go p = do r <- generatePrivKey
              return $ Left PartialProof
                            { pproofInput = p
                            , pproofR     = r
                            }

-- | Now we have commitments for every real node we can compute root
--   challenge for node and derive challenges for rest of node.
computeRealChallenges
  :: (EC a)
  => ByteString
  -> SigmaE (ProofSim  a) (Either (PartialProof a) (AtomicProof a))
  -> SigmaE (Challenge a) (Either (PartialProof a) (AtomicProof a))
computeRealChallenges message expr0 = goReal rootChallenge expr0
  where
    rootChallenge = getProofRootChallenge expr0 message
    --
    goReal ch = \case
      Leaf RealS (Left p) -> Leaf ch $ Left p
      Leaf _ _ -> error $ "impossible happened: Simulated leaf or mismatch"
      --
      AND RealS es -> AND ch $ goReal ch <$> es
      AND _     _  -> error "Impossible happened: simulated AND"
      --
      OR RealS  es -> OR ch $ orChildren es
      OR _      _  -> error "Impossible happened: simulated OR"
      where
        orChildren children = do
          let challengeForReal = orChallenge ch [ c | SimulatedS c <- sexprAnn <$> children ]
              update e
                | RealS <- sexprAnn e = goReal challengeForReal e
                | otherwise           = goSim e
          fmap update children
    --
    goSim = \case
      Leaf (SimulatedS ch) p -> Leaf ch p
      Leaf _               _ -> error "Impossible happened: Real leaf or mismatch"
      --
      AND (SimulatedS ch) es -> AND ch $ goSim <$> es
      AND _               _  -> error "Impossible happened: real AND"
      --
      OR  (SimulatedS ch) es -> OR ch $ goSim <$> es
      OR  _               _  -> error "Impossible happened: real OR"

evaluateRealProof
  :: (EC a)
  => Env a
  -> SigmaE (Challenge a) (Either (PartialProof a) (AtomicProof a))
  -> SigmaE (Challenge a) (AtomicProof a)
evaluateRealProof env = go
  where
    go = \case
      Leaf ch (Right p) -> Leaf ch p
      Leaf ch (Left PartialProof{..}) -> case pproofInput of
        InputDLog dlog -> Leaf ch $ ProofDL ProofDLog
          { proofDLog'public      = dlog
          , proofDLog'commitmentA = publicKey pproofR
          , proofDLog'challengeE  = ch
          , proofDLog'responseZ   = z
          }
        InputDTuple dtuple -> Leaf ch $ ProofDT $ ProofDTuple
          { proofDTuple'public      = dtuple
          , proofDTuple'commitmentA = ( pproofR .*^ dtuple'g   dtuple
                                      , pproofR .*^ dtuple'g_x dtuple
                                      )
          , proofDTuple'challengeE  = ch
          , proofDTuple'responseZ   = z
          }
        where
          z = getResponseForInput env pproofR ch pproofInput
      AND ch es -> AND ch $ go <$> es
      OR  ch es -> OR  ch $ go <$> es

----------------------------------------------------------------
-- Old code
----------------------------------------------------------------

-- | Genererate simalated proofs and commitments for real proofs
-- Prover Steps 4, 5, and 6 together: find challenges for simulated nodes; simulate simulated leaves;
-- compute commitments for real leaves
generateCommitments
  :: (EC a)
  => SigmaE ProofVar (ProofInput a)
  -> Prove (SigmaE (ProofSim a) (Either (PartialProof a) (AtomicProof a)))
generateCommitments tree = case sexprAnn tree of
  Real      -> goReal tree
  -- Prover Step 2: If the root of the tree is marked "simulated" then the prover does not have enough witnesses
  -- to perform the proof. Abort.
  Simulated -> throwError "The root is simulated. Can not produce the proof."
  where
    -- Go down expecting real node
    goReal = \case
      Leaf Real k -> do r <- generatePrivKey
                        return $ Leaf RealS $ Left $ PartialProof
                          { pproofInput = k
                          , pproofR     = r
                          }
      AND Real es -> AND RealS <$> traverse goReal     es
      OR  Real es -> OR  RealS <$> traverse simulateOR es
        where
          simulateOR e = case sexprAnn e of
            Real      -> goReal e
            Simulated -> do ch <- liftIO generateChallenge
                            goSim ch e
      _ -> throwError "Simulated node!"
    --
    goSim ch = \case
      Leaf Simulated k      -> liftIO $ Leaf (SimulatedS ch) . Right <$> simulateAtomicProof k ch
      AND  Simulated es     -> AND (SimulatedS ch) <$> traverse (goSim ch) es
      OR   Simulated []     -> throwError "Empty OR"
      OR   Simulated (e:es) -> do esWithCh <- liftIO $ forM es $ \x -> (,x) <$> generateChallenge
                                  let ch0 = foldl xorChallenge ch $ map fst esWithCh
                                  OR (SimulatedS ch) <$> traverse (uncurry goSim) ((ch0,e) : esWithCh)
      _ -> throwError "Real node"

initRootChallenge
  :: (EC a, CBOR.Serialise (ECPoint a))
  => SigmaE k (FiatShamirLeaf a)
  -> ByteString
  -> Challenge a
initRootChallenge expr message =
  randomOracle $ (LB.toStrict $ CBOR.serialise $ toFiatShamir expr) <> message

getProofRootChallenge ::
     forall a . (EC a)
  => SigmaE (ProofSim a) (Either (PartialProof a) (AtomicProof a))
  -> ByteString
  -> Challenge a
getProofRootChallenge expr = initRootChallenge (extractCommitment <$> expr)
  where
    extractCommitment :: Either (PartialProof a) (AtomicProof a) -> FiatShamirLeaf a
    extractCommitment = either extractPartialProof extractAtomicProof

    extractPartialProof x = case pproofInput x of
      InputDLog dlog -> FiatShamirLeafDLog dlog  (publicKey rnd)
      InputDTuple dt -> FiatShamirLeafDTuple dt  ( rnd .*^ dtuple'g   dt
                                                 , rnd .*^ dtuple'g_x dt
                                                 )
      where
        rnd = pproofR x

    extractAtomicProof = \case
      ProofDL dlog   -> FiatShamirLeafDLog   (proofDLog'public dlog)     (proofDLog'commitmentA dlog)
      ProofDT dtuple -> FiatShamirLeafDTuple (proofDTuple'public dtuple) (proofDTuple'commitmentA dtuple)

getPrivateKeyForInput :: EC a => Env a -> ProofInput a -> PrivKey a
getPrivateKeyForInput (Env env) input =
  let [sk] = [ getSecretKey | KeyPair{..} <- env
                            , getPublicKey == pk
                            ]
  in  sk
  where
    pk = leafPublicKey input

getResponseForInput :: EC a => Env a -> ECScalar a -> Challenge a -> ProofInput a -> ECScalar a
getResponseForInput env rnd ch inp = rnd .+. (sk .*. fromChallenge ch)
  where
    sk = getPrivateKeyForInput env inp

generateProofs
  :: forall a. (EC a)
  => Env a
  -> SigmaE (ProofSim a) (Either (PartialProof a) (AtomicProof a))
  -> ByteString
  -> Prove (SigmaE (Challenge a) (AtomicProof a))
generateProofs (Env env) expr0 message = goReal ch0 expr0
  where
    -- Prover Steps 7: convert the relevant information in the tree (namely, tree structure, node types,
    -- the statements being proven and commitments at the leaves)
    -- to a string
    --
    -- Prover Step 8: compute the challenge for the root of the tree as the Fiat-Shamir hash of s
    -- and the message being signed.
    ch0 :: Challenge a
    ch0 = getProofRootChallenge expr0 message

    -- Prover Step 9: complete the proof by computing challenges at real
    -- nodes and additionally responses at real leaves
    goReal ch = \case
      Leaf RealS (Left PartialProof{..}) -> do
        let z = getResponseForInput (Env env) pproofR ch pproofInput
        return $ case pproofInput of
          InputDLog dlog ->
            Leaf ch $ ProofDL ProofDLog
              { proofDLog'public      = dlog
              , proofDLog'commitmentA = publicKey pproofR
              , proofDLog'challengeE  = ch
              , proofDLog'responseZ   = z
              }
          InputDTuple dtuple ->
            Leaf ch $ ProofDT $ ProofDTuple
              { proofDTuple'public      = dtuple
              , proofDTuple'commitmentA = ( pproofR .*^ dtuple'g   dtuple
                                          , pproofR .*^ dtuple'g_x dtuple
                                          )
              , proofDTuple'challengeE  = ch
              , proofDTuple'responseZ   = z
              }
      Leaf _ _ -> error $ "impossible happened: Simulated leaf or mismatch"
      --
      AND RealS es -> AND ch <$> traverse (goReal ch) es
      AND _     _  -> error "Impossible happened: simulated AND"
      --
      OR RealS  es -> OR ch <$> orChildren ch es
      OR _      _  -> error "Impossible happened: simulated OR"
      where
        orChildren rootCh children = do
          let challengeForReal = orChallenge rootCh
                [ c | SimulatedS c <- sexprAnn <$> children ]
              update e
                | RealS <- sexprAnn e = goReal challengeForReal e
                | otherwise           = goSim e
          traverse update children
    -- This step is not needed, but might be useful to prevent side-channel timing attacks.
    goSim = \case
      Leaf (SimulatedS ch) (Right pdl) -> pure $ Leaf ch pdl
      Leaf _               _           -> error "Impossible happened: Real leaf or mismatch"
      --
      AND (SimulatedS ch) es -> AND ch <$> traverse goSim es
      AND _               _  -> error "Impossible happened: real AND"
      --
      OR  (SimulatedS ch) es -> OR ch <$> traverse goSim es
      OR  _               _  -> error "Impossible happened: real OR"


orChallenge :: EC a => Challenge a -> [Challenge a] -> Challenge a
orChallenge ch rest = foldl xorChallenge ch rest

----------------------------------------------------------------
-- Verification
----------------------------------------------------------------

-- | Check that proof is correct.
verifyProof :: forall a. (EC a, Eq (Challenge a))
  => Proof a
  -> ByteString
  -> Bool
verifyProof proof message
  =  (getVerifyRootChallenge compTree message == proof'rootChallenge proof)
  && all verifyAtomicProof compTree
  where
    compTree = completeProvenTree proof

-- | In top-down traversal compute challenges for each leaf node in
--   tree and build 'AtomicProof' for it.
completeProvenTree :: EC a => Proof a -> SigmaE () (AtomicProof a)
completeProvenTree Proof{..} = go proof'rootChallenge proof'tree
  where
    go ch = \case
      ProvenLeaf resp proofInp -> Leaf () $ getAtomicProof ch proofInp resp
      ProvenOr leftmost rest   -> OR   () $ fmap (\OrChild{..} -> go orChild'challenge orChild'tree)
                                          $ getLeftmostOrChallenge ch leftmost rest : rest
      ProvenAnd children       -> AND  () $ go ch <$> children

    getAtomicProof ch proofInp respZ = case proofInp of
      InputDLog dlog -> ProofDL $ ProofDLog
        { proofDLog'public      = dlog
        , proofDLog'commitmentA = getCommitment respZ ch dlog
        , proofDLog'responseZ   = respZ
        , proofDLog'challengeE  = ch
        }
      InputDTuple dtuple -> ProofDT $ ProofDTuple
        { proofDTuple'public      = dtuple
        , proofDTuple'commitmentA = getCommitmentDTuple respZ ch dtuple
        , proofDTuple'responseZ   = respZ
        , proofDTuple'challengeE  = ch
        }

    getLeftmostOrChallenge ch leftmost children = OrChild
      { orChild'challenge = orChallenge ch (orChild'challenge <$> children)
      , orChild'tree      = leftmost
      }

getVerifyRootChallenge ::
     (EC a)
  => SigmaE k (AtomicProof a)
  -> ByteString
  -> Challenge a
getVerifyRootChallenge expr = initRootChallenge (extractFiatShamirLeaf <$> expr)
  where
    extractFiatShamirLeaf = \case
      ProofDL ProofDLog{..}   -> FiatShamirLeafDLog   proofDLog'public   proofDLog'commitmentA
      ProofDT ProofDTuple{..} -> FiatShamirLeafDTuple proofDTuple'public proofDTuple'commitmentA

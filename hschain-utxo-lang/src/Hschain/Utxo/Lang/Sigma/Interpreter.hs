{-# LANGUAGE ViewPatterns #-}
-- | The module defines functions to create proofs for sigma-expressions
-- and verify them.
--
-- Implemented by the paper: "ErgoScript, a Cryptocurrency Scripting Language
-- Supporting Noninteractive Zero-Knowledge Proofs" by Ergo Developers
module Hschain.Utxo.Lang.Sigma.Interpreter(
    Proof
  , createProof
  , verifyProof
  , verifyProofExpr
  , ProvenTree(..)
  , completeProvenTree
  , ProofTag(..)
  , ProofVar(..)
  , ProofSim(..)
  , Prove(..)
  , runProve
  , computeRootChallenge
  , orChallenge
  , toProof
  , markTree
  , ownsKey
  , getResponseForInput
  , CommitedProof(..)
  -- * New API
  , simulateProofs
  , makeLocalCommitements
  , computeRealChallenges
  , evaluateRealProof
  , Partial(..)
  ) where

import Codec.Serialise (Serialise, serialise)
import Control.Lens  hiding (children)
import Control.Monad
import Control.Monad.Except
import Control.DeepSeq

import Data.Aeson      (ToJSON,FromJSON)
import HSChain.Crypto              (PublicKey,CryptoAsymmetric(..))
import Data.ByteString (ByteString)
import Data.Set        (Set)
import Data.Text       (Text)
import Data.Functor.Classes (Eq2(..))
import qualified Data.ByteString.Lazy  as LB
import qualified Data.Set              as Set
import GHC.Generics (Generic)

import HSChain.Crypto              (PublicKey, PrivKey, CryptoAsymmetric(..), ByteRepr(..))
import Hschain.Utxo.Lang.Sigma.EllipticCurve
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

-- | Partial proof. We build proofs in several stages ant this means
--   at some point we have complete (simulated) proofs and partial
--   proofs at leaves.
data Partial f a
  = Complete (AtomicProof a)
  | Partial  (f a)
  deriving (Generic)


instance HasPK f => HasPK (Partial f) where
  getPK = \case Complete p -> getPK p
                Partial  p -> getPK p
instance HasCommitment f => HasCommitment (Partial f) where
  toCommitedProof = \case Partial  p -> toCommitedProof p
                          Complete p -> toCommitedProof p
instance HasCommitmentData f => HasCommitmentData (Partial f) where
  toCommitedData = \case Partial  p -> toCommitedData p
                         Complete p -> toCommitedData p


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

$(makePrisms ''Partial)
$(makePrisms ''ProofSim)

----------------------------------------------------------------
-- Creation of proofs
----------------------------------------------------------------

-- | Create proof for Σ-expression locally. No communication will be
--   needed. Function will use private keys from 'Env' in order to
--   compute necessary proofs
createProof :: (EC a)
  => Env a                    -- ^ List of keys for which we create
  -> SigmaE () (ProofInput a) -- ^ Σ-expression for which we create proof
  -> ByteString               -- ^ Message linked to sigma expression (TX hash)
  -> IO (Either Text (Proof a))
createProof env expr message = runProve $ do
  -- 1. Mark which leaves are real and which are simulated.
  let marked = markTree (isProvable env . getPK) expr
  -- 2. Generate simulated proofs in leaves marked as simulated
  simulated <- simulateProofs marked
  -- 3. Generate commitments for real leaves.
  committed <- makeLocalCommitements simulated
  -- 4. Compute challenges for all real nodes
  let challenged = computeRealChallenges message committed
  -- 5. Generate responses for real leaves and convert proof to normal
  --    form.
  case evaluateRealProof env challenged of
    Nothing -> throwError "No key"
    Just p  -> pure $ toProof p

ownsKey :: EC a => Set (PublicKey a) -> ProofInput a -> Bool
ownsKey knownPKs = checkKey . getPK
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
markTree provable = clean . check
  where
    -- Step 1: Bottom-to-top. Mark as real everything that could be
    --         proven
    check = \case
      Leaf () leaf | provable leaf -> Leaf Real      leaf
                   | otherwise     -> Leaf Simulated leaf
      AND  () es -> AND k es'
        where
          es' = check <$> es
          k | all ((==Real) . sexprAnn) es' = Real
            | otherwise                     = Simulated
      OR   () es -> OR k es'
        where
          es' = check <$> es
          k | any ((==Real) . sexprAnn) es' = Real
            | otherwise                     = Simulated
    -- Change some real nodes to simulated to make sure each node has
    -- the right number of simulated children.
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
  => SigmaE ProofVar (ProofInput a)
  -> m (SigmaE (ProofSim a) (Partial ProofInput a))
simulateProofs = goReal
  where
    goReal = \case
      Leaf Real leaf -> pure $ Leaf RealS (Partial leaf)
      AND  Real es   -> AND RealS <$> traverse goReal es
      OR   Real es   -> OR  RealS <$> traverse simulateOR es
        where
          simulateOR e = case sexprAnn e of
            Real      -> goReal e
            Simulated -> do ch <- liftIO generateChallenge
                            goSim ch e
      _ -> error "simulateProofs: simulated proof encountered"
    goSim ch = \case
      Leaf Simulated leaf   -> liftIO $ Leaf (SimulatedS ch) . Complete <$> simulateAtomicProof leaf ch
      AND  Simulated es     -> AND (SimulatedS ch) <$> traverse (goSim ch) es
      OR   Simulated []     -> error "simulateProofs: Empty OR"
      OR   Simulated (e:es) -> do
        esWithCh <- liftIO $ forM es $ \x -> (,x) <$> generateChallenge
        let ch0 = foldl xorChallenge ch $ fst <$> esWithCh
        OR (SimulatedS ch) <$> traverse (uncurry goSim) ((ch0,e) : esWithCh)
      _ -> error "simulateProofs: internal error"

-- | Generate commitments for all real leaves. Since we aren't sending
--   data anywhere we simply storing freshly generated @r@ in leaves
--   of tree.
makeLocalCommitements
  :: (EC a, MonadIO m)
  => SigmaE t (Partial ProofInput a)
  -> m (SigmaE t (Partial PartialProof a))
makeLocalCommitements = traverse . _Partial %%~ commit
  where
    commit p = do r <- generatePrivKey
                  pure $ PartialProof { pproofInput = computeCommitments r p
                                      , pproofR     = r
                                      }


-- | Generate challenges for every node in expression tree. To be able
--   to do so we need to know commitments for every leaf of expression.
computeRealChallenges
  :: (EC a, HasCommitment f)
  => ByteString
  -> SigmaE (ProofSim  a) (Partial f a)
  -> SigmaE (Challenge a) (Partial f a)
computeRealChallenges message expr0 = goReal rootChallenge expr0
  where
    rootChallenge = computeRootChallenge expr0 message
    --
    goReal ch = \case
      Leaf RealS (Partial p) -> Leaf ch $ Partial p
      Leaf _ _ -> error $ "impossible happened: Simulated leaf or mismatch"
      --
      AND RealS es -> AND ch $ goReal ch <$> es
      AND _     _  -> error "Impossible happened: simulated AND"
      --
      OR RealS  es -> OR ch $ orChildren es
      OR _      _  -> error "Impossible happened: simulated OR"
      where
        orChildren children = update <$> children
          where
            challengeForReal = orChallenge ch [ c | SimulatedS c <- sexprAnn <$> children ]
            update e
              | RealS <- sexprAnn e = goReal challengeForReal e
              | otherwise           = goSim e
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


-- | Compute responses to challenges in real leaves. In this step we
--   need access to private keys to compute responses @z@.
evaluateRealProof
  :: (EC a)
  => Env a
  -> SigmaE (Challenge a) (Partial PartialProof a)
  -> Maybe (SigmaE (Challenge a) (AtomicProof a))
evaluateRealProof env = traverseSigmaE $ \ch leaf -> case leaf of
  Complete p               -> pure p
  Partial PartialProof{..} -> do
    z <- getResponseForInput env pproofR ch (toProofInput pproofInput)
    pure $ case pproofInput of
      CommitedDLog dlog a -> ProofDL ProofDLog
        { proofDLog'public      = dlog
        , proofDLog'commitmentA = a
        , proofDLog'challengeE  = ch
        , proofDLog'responseZ   = z
        }
      CommitedDTuple dtuple a -> ProofDT $ ProofDTuple
        { proofDTuple'public      = dtuple
        , proofDTuple'commitmentA = a
        , proofDTuple'challengeE  = ch
        , proofDTuple'responseZ   = z
        }

-- | Purely syntactic step where we convert 'SigmaE' to 'Proof'
toProof :: SigmaE (Challenge a) (AtomicProof a) -> Proof a
toProof tree = Proof (sexprAnn tree) $ getProvenTree tree
  where
    getProvenTree = \case
      Leaf _ p  -> ProvenLeaf (responseZ p) (toProofInput p)
      AND  _ es -> ProvenAnd $ getProvenTree <$> es
      OR   _ es -> case es of
        []   -> error "toProof: No children for OR-node"
        a:as -> ProvenOr (getProvenTree a) (getOrRest <$> as)
      where
        getOrRest x = (sexprAnn x, getProvenTree x)


----------------------------------------------------------------
-- Helpers
----------------------------------------------------------------

computeRootChallenge
  :: (EC a, HasCommitment f)
  => SigmaE k (f a)
  -> ByteString
  -> Challenge a
computeRootChallenge expr message
  = randomOracle
  $ LB.toStrict (serialise $ bimap (const ()) toCommitedProof expr)
  <> message

getResponseForInput :: EC a => Env a -> ECScalar a -> Challenge a -> ProofInput a -> Maybe (ECScalar a)
getResponseForInput env r ch inp = do
  sk <- lookupSecret env (getPK inp)
  pure $! r .+. (sk .*. fromChallenge ch)

orChallenge :: EC a => Challenge a -> [Challenge a] -> Challenge a
orChallenge ch rest = foldl xorChallenge ch rest

----------------------------------------------------------------
-- Verification
----------------------------------------------------------------


-- | Check that proof is correct. To do so we compute commitments for
--   every node in Σ-expression tree compute Fiat-Shamir hash and
--   check that it's same as root challenge.
verifyProof
  :: (EC a, ByteRepr msg)
  => Proof a -- ^ Proof
  -> msg     -- ^ Message being verified by proof
  -> Bool
verifyProof proof (encodeToBS -> message) =
  computeRootChallenge compTree message == proof'rootChallenge proof
  where
    compTree = completeProvenTree proof

-- | Check that proof is correct and matches given Σ-expression.
verifyProofExpr
  :: (EC a, ByteRepr msg)
  => Proof a                  -- ^ Proof under consideration
  -> msg                      -- ^ Message being verified by proof
  -> SigmaE () (ProofInput a) -- ^ Σ-expression being proven
  -> Bool
verifyProofExpr proof (encodeToBS -> message) expr
  =  matches
  && computeRootChallenge compTree message == proof'rootChallenge proof
  where
    compTree = completeProvenTree proof
    -- Proof proves given
    matches  = liftEq2 (\_ _ -> True) (\inp p -> inp == toProofInput p)
      expr compTree


-- | In top-down traversal compute challenges for each leaf node in
--   tree and build 'AtomicProof' for it.
completeProvenTree :: EC a => Proof a -> SigmaE () (CommitedProof a)
completeProvenTree Proof{..} = go proof'rootChallenge proof'tree
  where
    go ch = \case
      ProvenLeaf z p         -> Leaf () $ computeLeafCommitments ch z p
      ProvenOr leftmost rest -> OR   () $  uncurry go
                                       <$> getLeftmostOrChallenge ch leftmost rest : rest
      ProvenAnd es           -> AND  () $ go ch <$> es
    -- Compute commitments for each leaf
    computeLeafCommitments ch z = \case
      InputDLog   pk -> CommitedDLog   pk (getCommitment       z ch pk)
      InputDTuple dh -> CommitedDTuple dh (getCommitmentDTuple z ch dh)
    -- Challenge for leftmost children of OR node is computed as xor
    -- of parent challenge rest of challenges
    getLeftmostOrChallenge ch leftmost children =
      ( orChallenge ch (fst <$> children)
      , leftmost
      )


----------------------------------------------------------------
-- Instances
----------------------------------------------------------------

deriving stock instance (EC a, Show (f a))    => Show (Partial f a)
deriving stock instance (EC a, Eq   (f a))    => Eq   (Partial f a)
instance (NFData (PublicKey a), NFData (Challenge a), NFData (PrivKey a), NFData (f a)
         ) => NFData (Partial f a)
instance (EC a, Serialise (f a)) => Serialise (Partial f a)
instance (EC a, ToJSON    (f a)) => ToJSON    (Partial f a)
instance (EC a, FromJSON  (f a)) => FromJSON  (Partial f a)

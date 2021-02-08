{-# LANGUAGE RankNTypes #-}
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
  , ProofVar(..)
  , ProofSim(..)
  , computeRootChallenge
  , toProof
  , markTree
  , getResponseForInput
  , CommitedProof(..)
  -- * New API
  , simulateProofs
  , makeLocalCommitements
  , computeRealChallenges
  , evaluateRealProof
  -- * Distributed proof creation
  -- $distributed
  -- ** Types
  , Partial(..)
  , DuringCommitment(..)
  , DuringChallenge(..)
    -- ** Main prover
  , mainStartProof
  , mainProcessCommitment
  , mainAdvanceToChallenge
  , mainProcessChallengeResponse
  , mainAdvanceToProof
    -- ** Subordinate prover
  , SubordinateProof(..)
  , proverGenerateCommitment
  , proverProcessChallenge
  ) where

import Codec.Serialise (Serialise, serialise)
import Control.Lens  hiding (children)
import Control.Monad
import Control.Monad.Except
import Control.DeepSeq

import Data.Aeson      (ToJSON,FromJSON)
import Data.Traversable
import Data.Bifunctor
import Data.ByteString (ByteString)
import Data.List.NonEmpty (NonEmpty(..))
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

deriving instance (EC a) => Show (ProofSim a)

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
createProof env expr message = runExceptT $ do
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
    --
    -- FIXME: Express mark everything but first as simulated more
    --        clearly (and hopefully possible to abstract)
    splitOR (e:|es) = case sexprAnn e of
      Simulated -> markSim e :| splitORL es
      Real      -> clean   e :| fmap markSim es
    splitORL []     = error "Impossible"
    splitORL (e:es) = case sexprAnn e of
      Simulated -> markSim e : splitORL es
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
      Leaf Simulated leaf    -> liftIO $ Leaf (SimulatedS ch) . Complete <$> simulateAtomicProof leaf ch
      AND  Simulated es      -> AND (SimulatedS ch) <$> traverse (goSim ch) es
      OR   Simulated (e:|es) -> do
        esWithCh <- liftIO $ for es $ \x -> (,x) <$> generateChallenge
        let ch0 = xorChallengesOf (each . _1) ch esWithCh
        OR (SimulatedS ch) <$> traverse (uncurry goSim) ((ch0,e) :| esWithCh)
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
            challengeForReal = xorChallengesOf (each . to sexprAnn . _SimulatedS) ch children
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
      Leaf _ p       -> ProvenLeaf (responseZ p) (toProofInput p)
      AND  _ es      -> ProvenAnd $ getProvenTree <$> es
      OR   _ (a:|as) -> ProvenOr (getProvenTree a) (getOrRest <$> as)
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

computeResponseForInput :: EC a => PrivKey a -> ECScalar a -> Challenge a -> ECScalar a
computeResponseForInput sk r ch
  = r .+. (sk .*. fromChallenge ch)


xorChallengesOf :: EC a => Fold s (Challenge a) -> Challenge a -> s -> Challenge a
xorChallengesOf l = foldlOf' l xorChallenge

----------------------------------------------------------------
-- Distributed proof
----------------------------------------------------------------

-- $distributed
--
-- Distributed proof building is needed when there are several provers
-- that create proof together. One of the provers is designated as
-- main prover and will drive process of creation proof. This process
-- requires two rounds of communication.
--
-- Proof modelled as state machines. One for main prover and one for
-- subordinate provers. Only transition functions are provided and
-- implementer is expected to define necessary data types.
--
-- All messages in protocol are directed to owner of public key and
-- generated by only single key. In case when prover possess several
-- keys he has to generate separate message for each key,
--
-- Before starting proof creation all provers should agree on
-- Σ-expression begin proven and message being signed by it. They will
-- be used by both main and subordinate provers.
--
-- 1. Main prover computes list of public keys involved in the proof
--    creation. Then he simulates some of nodes by calling
--    'mainStartProof' and send request for commitments to each public
--    key involved in proof. Request doesn't contain any information
--    about proof so it could be done as part of earlier negotiations.
--
-- 2. Each subordinate prover generates commitments for every leaf
--    proven by his key and keeps generated @r@ secret. Commitments
--    @a@ are sent to the main prover.
--
-- 3. Main prover collects commitments from subordinate provers and
--    once he has enough he generates challenges by calling
--    'mainAdvanceToChallenge'.
--
-- 4. Each subordinate prover verifies that challenges are correct
--    (generated from message and Σ-expression), computes response and
--    sends it to main prover using ('proverProcessChallenge').
--
-- 5. After collecting all responses main prover generates final
--    proof.
--
-- Note that merely observing such communication will reveal which
-- nodes are real and which are simulated.


-- | State of leaves for the main prover when we're collecting
--   commitments from other provers.
data DuringCommitment a
  = BeforeCommitment (ProofInput a)
    -- ^ We didn't get commitment yet
  | AfterCommitment  (CommitedProof a)
    -- ^ We got commit

-- | State of leaves for the main prover when we're waiting for other
--   provers' responses to challnges.
data DuringChallenge a
  = SentChallenge (CommitedProof a) -- ^ We've sent challenge to prover
  | GotResponce   (AtomicProof   a) -- ^ We've got response from prover

deriving instance EC a => Show (DuringChallenge  a)
deriving instance EC a => Show (DuringCommitment a)

instance HasPK DuringCommitment where
  getPK = \case BeforeCommitment p -> getPK p
                AfterCommitment  p -> getPK p

instance HasPK DuringChallenge where
  getPK = \case SentChallenge p -> getPK p
                GotResponce   p -> getPK p

$(makePrisms ''DuringCommitment)
$(makePrisms ''DuringChallenge)


----------------------------------------
-- State machine for main prover

-- | Main prover decides which nodes are simulated and which are real.
--   Then he request commitments from subordinate provers.
mainStartProof
  :: (EC a, MonadIO m, Ord (PublicKey a))
  => Set (PublicKey a)
  -- ^ Set of public keys that are available for proof.
  -> SigmaE () (ProofInput a)
  -- ^ Expression to prove.
  -> m (SigmaE (ProofSim a) (Partial DuringCommitment a))
mainStartProof knownKeys expr = do
  -- FIXME: Check provability
  e <- simulateProofs $ markTree canProve expr
  pure $ e & mapped . _Partial %~ BeforeCommitment
  where
    canProve input = getPK input `Set.member` knownKeys

-- | Add commitment message from subordinate prover to proof. Function
--   checks that commitments are correct and every key that should be
--   proven got commitment. It's not idempotent.
mainProcessCommitment
  :: (EC a)
  => PublicKey a
  -- ^ Key from which we received message
  -> SigmaE () (Maybe (CommitmentData a))
  -- ^ Commitments from subordinate prover
  -> SigmaE (ProofSim a) (Partial DuringCommitment a)
  -- ^ Proof being built
  -> Either Text (SigmaE (ProofSim a) (Partial DuringCommitment a))
mainProcessCommitment pk resp proof =
  case zipSigmaE merge proof resp of
    Nothing -> throwError "Response shape is not correct"
    Just e  -> sequence e
  where
    -- Leaves for different key shouldn't get a commitment
    merge _ p Nothing
      | pk /= getPK p = pure p
      | otherwise     = throwError "No commitment for key"
    -- All leaves for key in question should get commitment
    merge s p (Just c)
      | pk /= getPK p = throwError "Commitment for different key"
      | otherwise = case s of
          SimulatedS{} -> pure p
          RealS        -> case p of
            Complete _
              -> throwError "Internal error: mismatched leaf and annotation"
            Partial (BeforeCommitment q)
              -> Partial . AfterCommitment <$> mergeCmt q c
            Partial (AfterCommitment _)
              -> throwError "Double commitment"
    --
    mergeCmt (InputDLog    k) (CommitmentDL a) = pure $ CommitedDLog   k  a
    mergeCmt (InputDTuple dh) (CommitmentDT a) = pure $ CommitedDTuple dh a
    mergeCmt _ _ = throwError "Mismatched commitment"


-- | Attempt to advance to challenge phase. Function will succeed if
--   all commitments are received.
--
--   It generates challenges for each node and generate message to be
--   sent to each subordinate prover. They already made a commitment
--   so they know which nodes should get a responses.
--
--   We have to send all commitments to each prover so they could
--   compute challenges and ensure that they're proving correct
--   transaction. If main prover only sends them challenges for leaf
--   nodes he could coax rest provers to prove some other
--   Σ-expression.
mainAdvanceToChallenge
  :: (EC a, ByteRepr msg)
  => msg
     -- ^ Message being signed
  -> SigmaE (ProofSim a) (Partial DuringCommitment a)
     -- ^ Proof under construction
  -> Maybe ( SigmaE (Challenge a) (Partial DuringChallenge a)
           , SigmaE (Challenge a) (CommitedProof a)
           )
     -- ^ Pair of new state of proof and message for other provers.
mainAdvanceToChallenge (encodeToBS -> message) proof = do
  expr <- (traverse . _Partial %%~ preview _AfterCommitment) proof
  -- Compute challenges
  let challenged = computeRealChallenges message expr
  pure ( challenged & mapped . _Partial %~ SentChallenge
       , challenged & mapped            %~ toCommitedProof
       )

-- | Process response to the challenge for some prover. it checks that
--   response is given for all nodes that require it
mainProcessChallengeResponse
  :: (EC a)
  => PublicKey a
     -- ^ Key from which we received responses to challenge
  -> SigmaE () (Maybe (ECScalar a))
     -- ^ Responses to challenge
  -> SigmaE (Challenge a) (Partial DuringChallenge a)
     -- ^ Proof under construction
  -> Either Text (SigmaE (Challenge a) (Partial DuringChallenge a))
mainProcessChallengeResponse pk resp proof =
  case zipSigmaE merge proof resp of
    Nothing -> throwError "Wrong shape of response"
    Just e  -> sequence e
  where
    -- No response is correct
    merge _ leaf Nothing = case leaf of
      Complete{}              -> pure leaf
      Partial GotResponce{}   -> pure leaf
      Partial SentChallenge{}
        | getPK leaf /= pk    -> pure leaf
        | otherwise           -> throwError "No response for key"
    -- Response if correct
    merge ch leaf (Just z) = case leaf of
      Complete{}                -> throwError "Response to simulated node"
      Partial GotResponce{}     -> throwError "Response to proven node"
      Partial (SentChallenge p) -> pure $ Partial $ GotResponce $
        case p of
          CommitedDLog   k a -> ProofDL $ ProofDLog   k a ch z
          CommitedDTuple k a -> ProofDT $ ProofDTuple k a z  ch

-- | Check whether we collected all responses @z@ from each prover and
--   produce complete proof.
mainAdvanceToProof
  :: SigmaE (Challenge a) (Partial DuringChallenge a)
  -> Maybe (Proof a)
mainAdvanceToProof expr = do
  proof <- for expr $ \case
               Complete p              -> Just p
               Partial (GotResponce p) -> Just p
               Partial SentChallenge{} -> Nothing
  pure $ toProof proof


----------------------------------------------------------------
-- State machine for subordinate prover
----------------------------------------------------------------

-- | Proof as used by subordinate prover. It contain either plain
--   'ProofInput' which is being proven elsewhere or 'PartialProof'
--   which leaf that is being proven by that prover.
data SubordinateProof a
  = RemoteProof (ProofInput   a)
  | LocalProof  (PartialProof a)
  deriving (Generic)

-- | Subordinate prover processes request for commitments. It produces
--   new tree with partial proof. Former should be kept secret and
--   latter is response to be sent to main prover
proverGenerateCommitment
  :: (EC a, MonadIO m)
  => PublicKey a
     -- ^ Public key of prover
  -> SigmaE () (ProofInput a)
     -- ^ Sigma expression being proven
  -> m ( SigmaE () (SubordinateProof a)
       , SigmaE () (Maybe (CommitmentData a))
       )
proverGenerateCommitment pk expr = do
  commited <- traverse commit expr
  pure ( commited
       , commited <&> \case
           RemoteProof _ -> Nothing
           LocalProof  p -> Just $ toCommitedData p
       )
  where
    commit p
      | pk /= getPK p = pure $ RemoteProof p
      | otherwise     = do r <- generatePrivKey
                           pure $ LocalProof PartialProof
                             { pproofInput = computeCommitments r p
                             , pproofR     = r
                             }


-- | Now we process challenge. First we must ensure that we're signing
--   transaction we agreed to sign.
--
--   Under no circumstances prover must respond to two different
--   challenges to commitment since that will reveal private
--   key. Correct way of handling this is to discard secret @r@ before
--   sending reply to challenge.
proverProcessChallenge
  :: (EC a, ByteRepr msg)
  => PrivKey a
     -- ^ Private key of prover
  -> msg
     -- ^ Message being signed-
  -> SigmaE () (SubordinateProof a)
     -- ^ Sigma expression being proven
  -> SigmaE (Challenge a) (CommitedProof a)
     -- ^ Challenge from main prover
  -> Either Text (SigmaE () (Maybe (ECScalar a)))
proverProcessChallenge sk (encodeToBS -> message) commitments challenges = do
  -- 1. We need to check whether we signing correct message.
  unless (checkChallenges rootChallenge challenges)
    $ Left "Invalid challenge"
  -- 2. We need to check that message matches
  e <- case zipSigmaE merge challenges commitments of
    Nothing -> Left "Challenge shape mismatch"
    Just e  -> sequence e
  pure $ first (const ()) e
  where
    rootChallenge = computeRootChallenge challenges message
    --
    merge _  _ (RemoteProof _) = pure Nothing
    merge ch commited (LocalProof p)
      -- We must prove same expression
      | toProofInput commited /= toProofInput p
        = Left "Expressions don't match"
      -- If commitments don't mach node is simulated and we don't need
      -- to generate response
      | toCommitedData commited /= toCommitedData p
        = pure Nothing
      | otherwise
        = pure $ Just $ computeResponseForInput sk (pproofR p) ch


-- | Check that all challenges are correct
checkChallenges :: (EC a) => Challenge a -> SigmaE (Challenge a) b -> Bool
checkChallenges ch0 = \case
  Leaf ch _       -> ch0 == ch
  OR   ch (e:|es) -> ch0 == ch
                  && ch0 == xorChallengesOf (each . to sexprAnn) (sexprAnn e) es
                  && and [ checkChallenges (sexprAnn a) a | a <- e:es ]
  AND  ch es      -> ch0 == ch && all (checkChallenges ch) es


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
                                       <$> getLeftmostOrChallenge ch leftmost rest :| rest
      ProvenAnd es           -> AND  () $ go ch <$> es
    -- Compute commitments for each leaf
    computeLeafCommitments ch z = \case
      InputDLog   pk -> CommitedDLog   pk (getCommitment       z ch pk)
      InputDTuple dh -> CommitedDTuple dh (getCommitmentDTuple z ch dh)
    -- Challenge for leftmost children of OR node is computed as xor
    -- of parent challenge rest of challenges
    getLeftmostOrChallenge ch leftmost children =
      ( xorChallengesOf (each . _1) ch children
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

deriving stock instance (EC a)  => Show (SubordinateProof a)
deriving stock instance (EC a)  => Eq   (SubordinateProof a)
instance (NFData (PublicKey a), NFData (PrivKey a)) => NFData    (SubordinateProof a)
instance (EC a) => Serialise (SubordinateProof a)
instance (EC a) => ToJSON    (SubordinateProof a)
instance (EC a) => FromJSON  (SubordinateProof a)

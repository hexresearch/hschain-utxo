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
) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

import HSChain.Crypto.Classes.Hash (CryptoHashable(..), genericHashStep)
import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.FiatShamirTree
import Hschain.Utxo.Lang.Sigma.Protocol
import Hschain.Utxo.Lang.Sigma.Types

import qualified Codec.Serialise as CBOR
import Data.Foldable
import Data.Maybe
import Data.Monoid (All(..))
import Data.Sequence (Seq)
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Data.Sequence as Seq

-- import Debug.Trace
-- import Text.Show.Pretty

-----------------------------------------------------

-- | Prove monad
newtype Prove a = Prove (ExceptT Text IO a)
  deriving newtype (Functor, Monad, Applicative, MonadError Text, MonadIO)

-- | Run prove monad.
runProve :: Prove a -> IO (Either Text a)
runProve (Prove p) = runExceptT p

----------------------------------------------------

-- Whether we create real proof or simulate it
data ProofVar
  = Real          -- ^ real proof tag
  | Simulated     -- ^ simulated proof tag
  deriving (Show,Eq)

-- | Proof tag and challenge.
data ProofTag a = ProofTag
  { proofTag'flag      :: ProofVar
  , proofTag'challenge :: Maybe (Challenge a)
  }

deriving stock   instance Show (Challenge a) => Show (ProofTag a)

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

-- | Proof to reconstruct all chalenges from the root challenge.
data Proof a = Proof
  { proof'rootChallenge :: Challenge a   -- ^ root chalenge
  , proof'tree          :: ProvenTree a  -- ^ expression to prove
  } deriving (Generic)

instance (EC a) => CBOR.Serialise (Proof a)
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
      { provenLeaf'responceZ :: ECScalar a
      , provenLeaf'publicK   :: PublicKey a
      }
  | ProvenOr
      { provenOr'leftmost  :: ProvenTree a
      , provenOr'rest      :: Seq (OrChild a)
      } -- ^ we keep chalenges for all children but for the leftmost one.
  | ProvenAnd
      { provenAnd'children :: [ProvenTree a]
      } -- ^ chalenges are calculated
  deriving (Generic)

instance (EC a) => CBOR.Serialise (ProvenTree a)

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

instance ( CryptoHashable (ECScalar  a)
         , CryptoHashable (ECPoint   a)
         , CryptoHashable (Challenge a)
         ) => CryptoHashable (OrChild a) where
  hashStep = genericHashStep hashDomain


deriving stock    instance (Show   (ECPoint a), Show   (ECScalar a), Show   (Challenge a)) => Show   (OrChild a)
deriving stock    instance (Eq     (ECPoint a), Eq     (ECScalar a), Eq     (Challenge a)) => Eq     (OrChild a)
deriving stock    instance (Ord    (ECPoint a), Ord    (ECScalar a), Ord    (Challenge a)) => Ord    (OrChild a)
deriving anyclass instance (NFData (ECPoint a), NFData (ECScalar a), NFData (Challenge a)) => NFData (OrChild a)

-- | Create proof for sigma expression based on ownership of collection of keys (@Env@)
newProof :: (EC a, Eq (ECPoint a), CBOR.Serialise (ECPoint a))
  => Env a -> SigmaE () (PublicKey a) -> IO (Either Text (Proof a))
newProof env expr = runProve $ do
  commitments <- generateCommitments (markTree env expr)
  toProof =<< generateProofs env commitments

-- Syntactic step that performs a type conversion only
toProof :: SigmaE (ProofTag a) (ProofDL a) -> Prove (Proof a)
toProof tree = Prove $ ExceptT $ pure $ liftA2 Proof (getRootChallenge tree) (getProvenTree tree)
  where
    getRootChallenge =
      maybe (Left "No root challenge") Right . proofTag'challenge . sexprAnn

    getProvenTree ptree = case ptree of
      Leaf _ p  -> Right $ ProvenLeaf (responseZ p) (publicK p)
      AND _ es  -> ProvenAnd  <$> traverse getProvenTree es
      OR  _ es  -> case es of
        []   -> Left "No children for OR-node"
        a:as -> liftA2 ProvenOr (getOrleftmostChild a) (getOrRest as)
      where
        getOrleftmostChild = getProvenTree

        getOrRest xs = fmap Seq.fromList $ traverse go xs
          where
            go x = do
              ch <- maybe (Left err) Right $ proofTag'challenge $ sexprAnn x
              t  <- getProvenTree x
              return $ OrChild ch t

            err = "No challenge for OR child node"


-- Mark all nodes according to whether we can produce proof for them
markTree :: (EC a, Eq (ECPoint a)) => Env a -> SigmaE () (PublicKey a) -> SigmaE ProofVar (PublicKey a)
markTree (Env env) = clean . check
  where
    -- Prover Step 1: Mark as real everything the prover can prove
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
    -- Prover Step 3: Change some "real" nodes to "simulated" to make sure each node
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

-- | Genererate simalated proofs and commitments for real proofs
-- Prover Steps 4, 5, and 6 together: find challenges for simulated nodes; simulate simulated leaves;
-- compute commitments for real leaves
generateCommitments
  :: (EC a)
  => SigmaE ProofVar (PublicKey a)
  -> Prove (SigmaE (ProofTag a) (Either (PartialProof a) (ProofDL a)))
generateCommitments tree = case sexprAnn tree of
  Real      -> goReal tree
  -- Prover Step 2: If the root of the tree is marked "simulated" then the prover does not have enough witnesses
  -- to perform the proof. Abort.
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


generateProofs
  :: forall a. (EC a, Eq (ECPoint a), CBOR.Serialise (ECPoint a))
  => Env a
  -> SigmaE (ProofTag a) (Either (PartialProof a) (ProofDL a))
  -> Prove (SigmaE (ProofTag a) (ProofDL a))
generateProofs (Env env) expr0 = goReal ch0 expr0
  where
    withChallenge ch tag = tag { proofTag'challenge = Just ch }


    -- Prover Steps 7: convert the relevant information in the tree (namely, tree structure, node types,
    -- the statements being proven and commitments at the leaves)
    -- to a string
    --
    -- Prover Step 8: compute the challenge for the root of the tree as the Fiat-Shamir hash of s
    -- and the message being signed.
    ch0 :: Challenge a
    ch0 = fiatShamirCommitment $ toFiatShamir $ fmap extractCommitment expr0

    extractCommitment :: Either (PartialProof a) (ProofDL a) -> FiatShamirLeaf a
    extractCommitment =
      either
        (\x -> FiatShamirLeaf (pproofPK x) (pproofA x))
        (\x -> FiatShamirLeaf (publicK x)  (commitmentA x))

    -- Prover Step 9: complete the proof by computing challenges at real
    -- nodes and additionally responses at real leaves
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
        _                      -> error $ "impossible happened in Sigma/Interpreter"
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

orChallenge :: EC a => Challenge a -> [Challenge a] -> Challenge a
orChallenge ch rest = foldl xorChallenge ch rest

-------------------------------------------------
-- verification

-- | Verify proof. It checks if the proof is correct.
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

-- | Calculate all challenges for all nodes of a proof.
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

{- For debug

traceMsg :: Show a => String -> a -> a
traceMsg msg a = trace (mconcat [msg, ": ", ppShow a]) a
-}

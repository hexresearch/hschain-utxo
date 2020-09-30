-- | Multiple provers create proof.
--
-- If several provers create joined proof we should have main prover.
-- Main prover first marks tree with Real or simulated tags and generates simulated proofs
-- for simulated leaves and branches of the expression tree.
--
-- Then main prover asks the rest of the provers for commitment for each leaf that do not belong to him.
-- Other provers generate random guess annd send commitment to main prover but keep randum guess as a secret.
-- Based on all commitments main prover generates all challenges. Challenges are created in deterministic manner
-- based on message that is signed shape of the sigma-expression and received commitments.
--
-- When all challenges have been created main prover asks for responces, that are based on challenges and
-- random secrets. After that step main prover has all information that is needed to complete the proof.
--
-- Note that other provers should watch out and check challenges that were generated to see for themselves
-- that main prover signed message that they expect.
--
-- Let's define the main steps with functions that are defined in this module:
--
-- > TODO
module Hschain.Utxo.Lang.Sigma.MultiSig(
    checkChallenges
  , generateSimulatedProofs
  , getChallenges
  , CommitmentQuery(..)
  , CommitmentSecret(..)
  , CommitmentResult(..)
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.FiatShamirTree
import Hschain.Utxo.Lang.Sigma.Protocol
import Hschain.Utxo.Lang.Sigma.Types
import Hschain.Utxo.Lang.Sigma.Interpreter

import Data.ByteString (ByteString)
import Data.Maybe
import Data.Set (Set)

import qualified Data.List as L
import qualified Data.Set as Set


type ProofExpr leaf a = SigmaE (ProofTag a) (Either (leaf a) (ProofDL a))

-- | Checks that right message was signed. Main prover uses the same message as me.
checkChallenges :: EC a => CommitmentSecretExpr a -> ChallengeExpr a -> ByteString -> Bool
checkChallenges expr message = undefined

generateSimulatedProofs
  :: EC a
  => SigmaE ProofVar (PublicKey a)
  -> Prove (SigmaE (ProofTag a) (Either (PublicKey a) (ProofDL a)))
generateSimulatedProofs tree = case sexprAnn tree of
  Real -> goReal tree
  -- Prover Step 2: If the root of the tree is marked "simulated" then the prover does not have enough witnesses
  -- to perform the proof. Abort.
  Simulated -> throwError "The root is simulated. Can not produce the proof."
  where
    realTag = ProofTag Real Nothing

    goReal = \case
      Leaf Real k  -> return $ Leaf realTag $ Left k
      AND  Real es -> AND realTag <$> traverse goReal     es
      OR   Real es -> OR  realTag <$> traverse simulateOR es
        where
          simulateOR e = case sexprAnn e of
            Real      -> goReal e
            Simulated -> do ch <- liftIO generateChallenge
                            goSim ch e
      _ -> throwError "Simulated node!"

    goSim ch = \case
      Leaf Simulated k      -> liftIO $ Leaf (ProofTag Simulated $ Just ch) . Right <$> simulateProofDL k ch
      AND  Simulated es     -> AND  (ProofTag Simulated $ Just ch) <$> traverse (goSim ch) es
      OR   Simulated []     -> throwError "Empty OR"
      OR   Simulated (e:es) -> do esWithCh <- liftIO $ forM es $ \x -> (,x) <$> generateChallenge
                                  let ch0 = foldl xorChallenge ch $ map fst esWithCh
                                  OR (ProofTag Simulated $ Just ch) <$> traverse (uncurry goSim) ((ch0,e) : esWithCh)
      _ -> throwError "Real node"

type CommitmentQueryExpr a  = SigmaE (ProofTag a) (Either (CommitmentQuery a)  (ProofDL a))
type CommitmentSecretExpr a = SigmaE (ProofTag a) (Either (CommitmentSecret a) (ProofDL a))

data CommitmentQuery a = CommitmentQuery
  { comQuery'publicKey  :: PublicKey a
  , comQuery'commitment :: Maybe (Commitment a)
  }

data CommitmentSecret a = CommitmentSecret
  { comSecret'query      :: CommitmentQuery a
  , comSecret'secret     :: Maybe (ECScalar a)
  }

data CommitmentResult a = CommitmentResult
  { comResult'publicKey  :: PublicKey a
  , comResult'commitment :: Commitment a
  }

-- | Prover sends request to other party to fill randomness in the expression leaves.
-- If partner founds key that he owns, he generates randomness and commitment
-- and keeps randomness as secret to himself and fills commitment to the expression-tree.
--
-- Result of execution contains pair of public expression filled with commitments
-- and private expression filled with secrets. We can use it to generate responses for the
-- final step of sigma-protocol dialog.
queryCommitments
  :: forall a . EC a
  => Set (PublicKey a) -> CommitmentQueryExpr a -> Prove (CommitmentQueryExpr a, CommitmentSecretExpr a)
queryCommitments knownKeys tree = fmap splitCommitmentAndSecret $ go tree
  where
    ownsKey query = Set.member (comQuery'publicKey query) knownKeys

    go :: CommitmentQueryExpr a -> Prove (CommitmentSecretExpr a)
    go = \case
      -- if we own the key we generate randomness and commitment based on it
      Leaf (ProofTag Real ch) (Left query) | ownsKey query -> do
                        r <- liftIO generateScalar
                        return $ Leaf (ProofTag Real ch) $ Left $ CommitmentSecret
                          { comSecret'query  = query { comQuery'commitment = Just $ fromGenerator r }
                          , comSecret'secret = Just r
                          }
      -- if we do not own the key we just copy query (it's someone elses commitment) and leave secret blank
      Leaf (ProofTag Real ch) (Left query) -> return $ Leaf (ProofTag Real ch) $ Left CommitmentSecret
                          { comSecret'query  = query
                          , comSecret'secret = Nothing
                          }
      AND tag es -> AND tag <$> traverse go es
      OR  tag es -> OR  tag <$> traverse go es

    splitCommitmentAndSecret expr = (eraseSecrets expr, expr)

    eraseSecrets :: CommitmentSecretExpr a -> CommitmentQueryExpr a
    eraseSecrets = \case
      Leaf tag eSecret -> Leaf tag $ either (Left . comSecret'query) Right eSecret
      AND  tag es      -> AND tag $ fmap eraseSecrets es
      OR   tag es      -> OR  tag $ fmap eraseSecrets es

appendCommitments
  :: EC a
  => CommitmentQueryExpr a -> CommitmentQueryExpr a -> Maybe (CommitmentQueryExpr a)
appendCommitments = appendProofExprBy appendComQueries
  where
    appendComQueries a b
      | comQuery'publicKey a == comQuery'publicKey b = Just $ a { comQuery'commitment = comQuery'commitment a <|> comQuery'commitment b }
      | otherwise                                    = Nothing

toCommitmentExpr :: CommitmentQueryExpr a -> Prove (CommitmentExpr a)
toCommitmentExpr tree = case tree of
  Leaf tag a -> Leaf tag <$> either (fmap Left . toCommitmentResult) (pure . Right) a
  AND tag as -> AND tag  <$> mapM toCommitmentExpr as
  OR  tag as -> OR  tag  <$> mapM toCommitmentExpr as
  where
    toCommitmentResult CommitmentQuery{..} = maybe noCommitmentError (\com -> pure $ CommitmentResult comQuery'publicKey com) comQuery'commitment

    noCommitmentError = throwError "No commitment found"

type ChallengeExpr  a = SigmaE (ProofTag a) (Either (ChallengeResult a) (ProofDL a))
type CommitmentExpr a = SigmaE (ProofTag a) (Either (CommitmentResult a) (ProofDL a))

data ChallengeResult a = ChallengeResult
  { challengeResult'publicKey  :: PublicKey a
  , challengeResult'commitment :: Commitment a
  , challengeResult'challenge  :: Challenge a
  }

getChallenges :: EC a => CommitmentExpr a -> ByteString -> Prove (ChallengeExpr a)
getChallenges expr0 message = goReal ch0 expr0
  where
    -- Prover Step 8: compute the challenge for the root of the tree as the Fiat-Shamir hash of s
    -- and the message being signed.
    ch0 = getRootChallengeBy extractCommitment expr0 message

    extractCommitment =
      either
        (\x -> FiatShamirLeaf (comResult'publicKey x) (comResult'commitment x))
        (\x -> FiatShamirLeaf (publicK x)  (commitmentA x))

    withChallenge ch tag = tag { proofTag'challenge = Just ch }

    -- Prover Step 9: complete the proof by computing challenges at real
    -- nodes and additionally responses at real leaves
    goReal ch = \case
      Leaf tag eProof -> case (proofTag'flag tag, eProof) of
        (Real, (Left CommitmentResult{..})) ->
          return $ Leaf (ProofTag Real $ Just ch) $ Left $ ChallengeResult {
                                      challengeResult'publicKey  = comResult'publicKey
                                    , challengeResult'commitment = comResult'commitment
                                    , challengeResult'challenge  = ch
                                    }
        (Simulated, Right e)   -> return $ Leaf (ProofTag Simulated $ Just ch) $ Right e
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
        (Simulated, Right pdl)  -> return $ Leaf tag $ Right pdl
        _                       -> err
      AND tag es     -> case proofTag'flag tag of
        Simulated  -> AND tag <$> traverse goSim es
        _          -> err
      OR tag es      -> case proofTag'flag tag of
        Simulated  -> OR tag <$> traverse goSim es
        _          -> err
      where
        err = throwError "Simulated parent node has real children"

data ResponseQuery a = ResponseQuery
  { responseQuery'publicKey  :: PublicKey a
  , responseQuery'commitment :: Commitment a
  , responseQuery'challenge  :: Challenge a
  , responseQuery'response   :: Maybe (Response a)
  }

queryToProofDL :: ResponseQuery a -> Maybe (ProofDL a)
queryToProofDL ResponseQuery{..} = fmap (\resp -> ProofDL
  { publicK     = responseQuery'publicKey
  , commitmentA = responseQuery'commitment
  , responseZ   = resp
  , challengeE  = responseQuery'challenge
  }) responseQuery'response


type ResponseQueryExpr a = SigmaE (ProofTag a) (Either (ResponseQuery a) (ProofDL a))

queryResponses :: EC a => Env a -> CommitmentSecretExpr a -> ChallengeExpr a -> Prove (ResponseQueryExpr a)
queryResponses env secretExpr expr = case (secretExpr, expr) of
  (Leaf _ (Left secretLeaf), Leaf tag (Left ChallengeResult{..})) -> return $ Leaf tag $ Left $ ResponseQuery
    { responseQuery'publicKey  = challengeResult'publicKey
    , responseQuery'commitment = challengeResult'commitment
    , responseQuery'challenge  = challengeResult'challenge
    , responseQuery'response   = liftA2 (toResp challengeResult'challenge) (getSecretRand secretLeaf) (getPrivateKey challengeResult'publicKey)
    }
  (Leaf _ _, Leaf tag (Right pdl)) -> return $ Leaf tag $ Right pdl
  (AND _ as, AND tag bs) -> fmap (AND tag) $ zipWithM (queryResponses env) as bs
  (OR  _ as, OR  tag bs) -> fmap (OR  tag) $ zipWithM (queryResponses env) as bs
  _                      -> throwError "Expression trees are different. Failed to generate response"
  where
    getPrivateKey pubKey = fmap secretKey $ L.find ((== pubKey) . publicKey) (unEnv env)

    getSecretRand CommitmentSecret{..} = comSecret'secret

    toResp challenge rand (Secret privKey) = z
      where
        e = fromChallenge challenge
        z = rand .+. (privKey .*. e)

appendResponses :: EC a => ResponseQueryExpr a  -> ResponseQueryExpr a -> Maybe (ResponseQueryExpr a)
appendResponses = appendProofExprBy app
  where
    app a b
      | same a b  = Just $ mixResponse a b
      | otherwise = Nothing

    mixResponse a b = a { responseQuery'response = responseQuery'response a <|> responseQuery'response b }

    same a b =
           responseQuery'publicKey a  == responseQuery'publicKey b
        && responseQuery'commitment a == responseQuery'commitment b
        && responseQuery'challenge a  == responseQuery'challenge b


responsesToProof :: ResponseQueryExpr a -> Prove (Proof a)
responsesToProof expr = toProof =<< go expr
  where
    go = \case
      Leaf tag e -> fmap (Leaf tag) $ either (maybe noResp pure . queryToProofDL) pure e
      AND tag as -> fmap (AND tag) $ mapM go as
      OR  tag as -> fmap (OR  tag) $ mapM go as

    noResp = throwError "Failed to find response"

appendProofExprBy :: EC a => (leaf a -> leaf a -> Maybe (leaf a)) -> ProofExpr leaf a  -> ProofExpr leaf a -> Maybe (ProofExpr leaf a)
appendProofExprBy app treeA treeB = case (treeA, treeB) of
  (Leaf tagA a, Leaf _ b)  -> fmap (Leaf tagA) (appendLeaves a b)
  (AND  tagA as, AND _ bs) -> fmap (AND tagA) (zipWithM rec as bs)
  (OR   tagA as, OR  _ bs) -> fmap (OR  tagA) (zipWithM rec as bs)
  _                        -> Nothing
  where
    rec = appendProofExprBy app

    appendLeaves a b = case (a, b) of
      (Left commitmentA, Left commitmentB)            -> fmap Left $ app commitmentA commitmentB
      (Right proofA, Right proofB) | proofA == proofB -> Just $ Right proofA
      _                                               -> Nothing


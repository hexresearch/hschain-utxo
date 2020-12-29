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
-- Main prover should know all public keys of the participants. First stage of the proof
-- is to mark tree nodes as real or simulated proof and generate all simulated proofs:
--
-- > commitmentQueryExpr <- initMultiSigProof knownKeys expr
--
-- We get special expression-tree that we can pass to other partners so that they can
-- provide their commitments. All participants call the function @queryCommitments@.
-- The result is a pair of public data with filled commitments and private data with secrets
-- that correspond to commitments.
--
-- > (commitmentExpr, secretExpr) <- queryCommitments myKeys querryExpr
--
-- Partner should pass @commitmentExpr@ to the main prover but keep @secretExpr@ private.
-- We need @secretExpr@ on the final round of signature to create responses.
--
-- Main prover collects all commitments and joins them with corresponding keys and applies @appendCommitments@.
-- For each commitment we keep only commitments for specific keys. This way we guarantee that partner
-- has signed only his own keys and somebodyelses. With @appendCommitments@ we join all commitments to single expression.
--
-- > commitmentExpr <- appendCommitments $ zip partnerKeys partnerCommitmments
--
-- With all suuplied commitments @commitmentExpr@ we can calculate all challenges with function @getChallenges@.
--
-- > challengeExpr <- getChallenges commitmentExpr message
--
-- We get the expression-tree with all challenges for all participants. Now we can query responces.
-- We give the expression of challenges to participant and he provides needed responces. He should use
-- the expression of secrets that he created on the second stage of this algorithm (when main prover asks for commitments).
--
-- > responseExpr <- queryResponses privateKeyEnv secretExpr challengeExpr
--
-- For this stage we need to know private keys. We use @privateKeyEnv@ to do it.
-- Main priver collects all repsonces and joins them with functions @filterResponses@ and @appendResponses@
-- just like with commitments.
--
-- > responseExpr <- toResponseExpr =<< (appendResponses $ zipWith filterResponses partnerKeys partnerResponses)
--
-- At least we can create the proof with function @responsesToProof@. It completes the algorithm.
--
-- multiSigProof = responsesToProof responseExpr
module Hschain.Utxo.Lang.Sigma.MultiSig(
    initMultiSigProof
  , queryCommitments
  , appendCommitments
  , toCommitmentExpr
  , checkChallenges
  , generateSimulatedProofs
  , getChallenges
  , CommitmentQueryExpr
  , CommitmentExpr
  , CommitmentSecretExpr
  , ChallengeExpr
  , ResponseQueryExpr
  , queryResponses
  , appendResponsesToProof
) where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Except

import Data.ByteString (ByteString)
import Data.Bifunctor
import Data.Either.Extra
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)

import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.FiatShamirTree
import Hschain.Utxo.Lang.Sigma.Protocol
import Hschain.Utxo.Lang.Sigma.Types
import Hschain.Utxo.Lang.Sigma.Interpreter

import qualified Data.List as L
import qualified Data.Set as Set

-- | First stage of multi-sig proof. Main prover marks expression tree nodes and leaves
-- as real and simulated proof and generates simulated proofs.
--
-- The result is expression tree for to query commitments for all participants of the signature.
-- Note that first argument contains all public keys of the participants (not only keys of the main prover).
-- for this stage we do not need to know the private keys but we should list all public keys
-- of the participants.
initMultiSigProof :: EC a
  => Set (PublicKey a)
  -> SigmaE () (PublicKey a)
  -> Prove (CommitmentQueryExpr a)
initMultiSigProof knownKeys expr =
  fmap toComQueryExpr $ generateSimulatedProofs $ markTree knownKeys expr
  where
    toComQueryExpr = fmap (either (Left . toComQuery) Right)

    toComQuery key = CommitmentQuery
      { comQuery'publicKey  = key
      , comQuery'commitment = Nothing
      }

type ProofExpr leaf a = SigmaE (ProofTag a) (Either (leaf a) (ProofDL a))

-- | Checks that right message was signed. Main prover uses the same message as me.
checkChallenges :: EC a => CommitmentExpr a -> ChallengeExpr a -> ByteString -> Bool
checkChallenges commitments expectedCommitments message =
  getChallenges commitments message == Right expectedCommitments

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

type CommitmentQueryExpr a  = ProofExpr CommitmentQuery a
type CommitmentSecretExpr a = ProofExpr CommitmentSecret a

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
      Leaf tag (Left query) | ownsKey query -> do
                        r <- liftIO generateScalar
                        return $ Leaf tag $ Left $ CommitmentSecret
                          { comSecret'query  = query { comQuery'commitment = Just $ fromGenerator r }
                          , comSecret'secret = Just r
                          }
      -- if we do not own the key we just copy query (it's someone elses commitment) and leave secret blank
      Leaf tag (Left query) -> return $ Leaf tag $ Left CommitmentSecret
                          { comSecret'query  = query
                          , comSecret'secret = Nothing
                          }
      Leaf tag (Right pdl) -> return $ Leaf tag $ Right pdl
      AND tag es -> AND tag <$> traverse go es
      OR  tag es -> OR  tag <$> traverse go es

    splitCommitmentAndSecret expr = (eraseSecrets expr, expr)

    eraseSecrets :: CommitmentSecretExpr a -> CommitmentQueryExpr a
    eraseSecrets = \case
      Leaf tag eSecret -> Leaf tag $ either (Left . comSecret'query) Right eSecret
      AND  tag es      -> AND tag $ fmap eraseSecrets es
      OR   tag es      -> OR  tag $ fmap eraseSecrets es

-- | Erase commitments for keys that we do not own.
-- We should apply it before appending commitments so that partners could not cheat on somebody else's keys.
filterCommitments :: EC a => Set (PublicKey a) -> CommitmentQueryExpr a -> CommitmentQueryExpr a
filterCommitments knownKeys = \case
  Leaf tag leaf -> Leaf tag $ first (\query ->
    if ownsPk (comQuery'publicKey query) knownKeys
      then query
      else eraseCommitment query
    ) leaf
  AND  tag as -> AND tag $ fmap rec as
  OR   tag as -> OR  tag $ fmap rec as
  where
    rec = filterCommitments knownKeys

    eraseCommitment q = q { comQuery'commitment = Nothing }

ownsPk :: EC a => PublicKey a -> Set (PublicKey a) -> Bool
ownsPk key knownKeys = Set.member key knownKeys

appendCommitments :: EC a => [(Set (PublicKey a), CommitmentQueryExpr a)] -> Prove (CommitmentExpr a)
appendCommitments exprs = case fmap (uncurry filterCommitments) exprs of
  []   -> throwError "List of commitments is empty"
  a:as -> toCommitmentExpr =<< (liftEither $ maybeToEither commitmentsDoNotMatch $ foldM appendCommitment2 a as)
  where
    commitmentsDoNotMatch = "Commitmnt expressions do not match"

appendCommitment2
  :: EC a
  => CommitmentQueryExpr a -> CommitmentQueryExpr a -> Maybe (CommitmentQueryExpr a)
appendCommitment2 = appendProofExprBy appendComQueries
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

type ChallengeExpr  a = ProofExpr ChallengeResult a
type CommitmentExpr a = ProofExpr CommitmentResult a

data ChallengeResult a = ChallengeResult
  { challengeResult'publicKey  :: PublicKey a
  , challengeResult'commitment :: Commitment a
  , challengeResult'challenge  :: Challenge a
  }

deriving stock   instance (Eq (ECPoint a), Eq (Challenge a)) => Eq (ChallengeResult a)

getChallenges :: EC a => CommitmentExpr a -> ByteString -> Either Text (ChallengeExpr a)
getChallenges expr0 message = goReal ch0 expr0
  where
    -- Prover Step 8: compute the challenge for the root of the tree as the Fiat-Shamir hash of s
    -- and the message being signed.
    ch0 = getRootChallengeBy extractCommitment expr0 message

    extractCommitment =
      either
        (\x -> FiatShamirLeaf (comResult'publicKey x)      (comResult'commitment x))
        (\x -> FiatShamirLeaf (dlog'publicKey $ publicK x) (commitmentA x))

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
queryToProofDL ResponseQuery{..} = fmap (\resp -> AtomicProof
  { publicK     = DLog responseQuery'publicKey
  , commitmentA = responseQuery'commitment
  , responseZ   = resp
  , challengeE  = responseQuery'challenge
  }) responseQuery'response


type ResponseQueryExpr a = ProofExpr ResponseQuery a

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

filterResponses :: EC a => Set (PublicKey a) -> ResponseQueryExpr a -> ResponseQueryExpr a
filterResponses knownKeys = \case
  Leaf tag leaf -> Leaf tag $ first (\query ->
    if ownsPk (responseQuery'publicKey query) knownKeys
      then query
      else eraseResponse query
    ) leaf
  AND  tag as -> AND tag $ fmap rec as
  OR   tag as -> OR  tag $ fmap rec as
  where
    rec = filterResponses knownKeys

    eraseResponse q = q { responseQuery'response = Nothing }

appendResponsesToProof :: EC a => [(Set (PublicKey a), ResponseQueryExpr a)] -> Prove (Proof a)
appendResponsesToProof resps = case fmap (uncurry filterResponses) resps of
  []   -> throwError "List of responses is empty"
  a:as -> responsesToProof =<< (liftEither $ maybeToEither responsesDoNotMatch $ foldM appendResponse2 a as)
  where
    responsesDoNotMatch = "Responses expressions do not match"

appendResponse2 :: EC a => ResponseQueryExpr a -> ResponseQueryExpr a -> Maybe (ResponseQueryExpr a)
appendResponse2 = appendProofExprBy app
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


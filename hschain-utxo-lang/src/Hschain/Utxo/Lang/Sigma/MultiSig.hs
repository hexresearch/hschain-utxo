-- | Multiple provers create proof.
--
-- If several provers create joined proof we should have main prover.
-- Main prover first marks tree with Real or simulated tags and
-- generates simulated proofs for simulated leaves and branches of the
-- expression tree.
--
-- Then main prover asks the rest of the provers for commitment for
-- each leaf that do not belong to him.  Other provers generate random
-- guess annd send commitment to main prover but keep randum guess as
-- a secret.  Based on all commitments main prover generates all
-- challenges. Challenges are created in deterministic manner based on
-- message that is signed shape of the sigma-expression and received
-- commitments.
--
-- When all challenges have been created main prover asks for
-- responces, that are based on challenges and random secrets. After
-- that step main prover has all information that is needed to
-- complete the proof.
--
-- Note that other provers should watch out and check challenges that
-- were generated to see for themselves that main prover signed
-- message that they expect.
--
-- Let's define the main steps with functions that are defined in this
-- module: Main prover should know all public keys of the
-- participants. First stage of the proof is to mark tree nodes as
-- real or simulated proof and generate all simulated proofs:
--
-- > commitmentQueryExpr <- initMultiSigProof knownKeys expr
--
-- We get special expression-tree that we can pass to other partners
-- so that they can provide their commitments. All participants call
-- the function @queryCommitments@.  The result is a pair of public
-- data with filled commitments and private data with secrets that
-- correspond to commitments.
--
-- > (commitmentExpr, secretExpr) <- queryCommitments myKeys querryExpr
--
-- Partner should pass @commitmentExpr@ to the main prover but keep
-- @secretExpr@ private.  We need @secretExpr@ on the final round of
-- signature to create responses.
--
-- Main prover collects all commitments and joins them with
-- corresponding keys and applies @appendCommitments@.  For each
-- commitment we keep only commitments for specific keys. This way we
-- guarantee that partner has signed only his own keys and
-- somebodyelses. With @appendCommitments@ we join all commitments to
-- single expression.
--
-- > commitmentExpr <- appendCommitments $ zip partnerKeys partnerCommitmments
--
-- With all suuplied commitments @commitmentExpr@ we can calculate all
-- challenges with function @getChallenges@.
--
-- > challengeExpr <- getChallenges commitmentExpr message
--
-- We get the expression-tree with all challenges for all
-- participants. Now we can query responces.  We give the expression
-- of challenges to participant and he provides needed responces. He
-- should use the expression of secrets that he created on the second
-- stage of this algorithm (when main prover asks for commitments).
--
-- > responseExpr <- queryResponses privateKeyEnv secretExpr challengeExpr
--
-- For this stage we need to know private keys. We use @privateKeyEnv@
-- to do it.  Main priver collects all repsonces and joins them with
-- functions @filterResponses@ and @appendResponses@ just like with
-- commitments.
--
-- > responseExpr <- toResponseExpr =<< (appendResponses $ zipWith filterResponses partnerKeys partnerResponses)
--
-- At least we can create the proof with function
-- @responsesToProof@. It completes the algorithm.
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

import HSChain.Crypto (PublicKey,CryptoAsymmetric(..))
import Hschain.Utxo.Lang.Sigma.DLog
import Hschain.Utxo.Lang.Sigma.DTuple
import Hschain.Utxo.Lang.Sigma.EllipticCurve
import Hschain.Utxo.Lang.Sigma.FiatShamirTree
import Hschain.Utxo.Lang.Sigma.Protocol
import Hschain.Utxo.Lang.Sigma.Interpreter

import qualified Data.List as L

-- | First stage of multi-sig proof. Main prover marks expression tree nodes and leaves
-- as real and simulated proof and generates simulated proofs.
--
-- The result is expression tree for to query commitments for all participants of the signature.
-- Note that first argument contains all public keys of the participants (not only keys of the main prover).
-- for this stage we do not need to know the private keys but we should list all public keys
-- of the participants.
initMultiSigProof :: EC a
  => Set (PublicKey a)
  -> SigmaE () (ProofInput a)
  -> Prove (CommitmentQueryExpr a)
initMultiSigProof knownKeys expr =
  fmap toComQueryExpr $ generateSimulatedProofs $ markTree knownKeys expr
  where
    toComQueryExpr = fmap (either (Left . toComQuery) Right)

    toComQuery = \case
      InputDLog x -> CommitmentQueryLog
        { comQuery'publicLog     = x
        , comQuery'commitmentLog = Nothing
        }
      InputDTuple x -> CommitmentQueryTuple
        { comQuery'publicTuple = x
        , comQuery'commitmentTuple = Nothing
        }

type ProofExpr leaf a = SigmaE (ProofTag a) (Either (leaf a) (AtomicProof a))

-- | Checks that right message was signed. Main prover uses the same message as me.
checkChallenges :: EC a => CommitmentExpr a -> ChallengeExpr a -> ByteString -> Bool
checkChallenges commitments expectedCommitments message =
  getChallenges commitments message == Right expectedCommitments

generateSimulatedProofs
  :: EC a
  => SigmaE ProofVar (ProofInput a)
  -> Prove (SigmaE (ProofTag a) (Either (ProofInput a) (AtomicProof a)))
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
      Leaf Simulated k      -> liftIO $ Leaf (ProofTag Simulated $ Just ch) . Right <$> simulateAtomicProof k ch
      AND  Simulated es     -> AND  (ProofTag Simulated $ Just ch) <$> traverse (goSim ch) es
      OR   Simulated []     -> throwError "Empty OR"
      OR   Simulated (e:es) -> do esWithCh <- liftIO $ forM es $ \x -> (,x) <$> generateChallenge
                                  let ch0 = L.foldl' xorChallenge ch $ map fst esWithCh
                                  OR (ProofTag Simulated $ Just ch) <$> traverse (uncurry goSim) ((ch0,e) : esWithCh)
      _ -> throwError "Real node"

type CommitmentQueryExpr a  = ProofExpr CommitmentQuery a
type CommitmentSecretExpr a = ProofExpr CommitmentSecret a

data CommitmentQuery a
  = CommitmentQueryLog
    { comQuery'publicLog     :: PublicKey a
    , comQuery'commitmentLog :: Maybe (Commitment a)
    }
  | CommitmentQueryTuple
    { comQuery'publicTuple     :: DTuple a
    , comQuery'commitmentTuple :: Maybe (Commitment a, Commitment a)
    }

data CommitmentSecret a = CommitmentSecret
  { comSecret'query      :: CommitmentQuery a
  , comSecret'secret     :: Maybe (ECScalar a)
  }

data CommitmentResult a
  = CommitmentResultLog
    { comResult'publicLog     :: PublicKey a
    , comResult'commitmentLog :: Commitment a
    }
  | CommitmentResultTuple
    { comResult'publicTuple     :: DTuple a
    , comResult'commitmentTuple :: (Commitment a, Commitment a)
    }

deriving stock   instance (Eq (ECPoint a), Eq (Challenge a)) => Eq (CommitmentResult a)

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
    go :: CommitmentQueryExpr a -> Prove (CommitmentSecretExpr a)
    go = \case
      -- if we own the key we generate randomness and commitment based on it
      Leaf tag (Left query) | ownsQuery knownKeys query -> do
                        rnd <- generatePrivKey
                        return $ Leaf tag $ Left $
                          case query of
                            CommitmentQueryLog{..} -> CommitmentSecret
                              { comSecret'query  = query { comQuery'commitmentLog = Just $ publicKey rnd }
                              , comSecret'secret = Just rnd
                              }
                            CommitmentQueryTuple{..} -> CommitmentSecret
                              { comSecret'query  = let dt = comQuery'publicTuple
                                                   in  query { comQuery'commitmentTuple = Just ( rnd .*^ dtuple'g   dt
                                                                                               , rnd .*^ dtuple'g_x dt
                                                                                               )
                                                             }
                              , comSecret'secret = Just rnd
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

ownsQuery :: EC a => Set (PublicKey a) -> CommitmentQuery a -> Bool
ownsQuery knownKeys query = ownsKey knownKeys (commitmentQueryInput query)
  where
    commitmentQueryInput = \case
      CommitmentQueryLog   dlog _   -> InputDLog dlog
      CommitmentQueryTuple dtuple _ -> InputDTuple dtuple


-- | Erase commitments for keys that we do not own.
-- We should apply it before appending commitments so that partners could not cheat on somebody else's keys.
filterCommitments :: EC a => Set (PublicKey a) -> CommitmentQueryExpr a -> CommitmentQueryExpr a
filterCommitments knownKeys = \case
  Leaf tag leaf -> Leaf tag $ first (\query ->
    if ownsQuery knownKeys query
      then query
      else eraseCommitment query
    ) leaf
  AND  tag as -> AND tag $ fmap rec as
  OR   tag as -> OR  tag $ fmap rec as
  where
    rec = filterCommitments knownKeys

    eraseCommitment q = case q of
      CommitmentQueryLog{..}   -> q { comQuery'commitmentLog   = Nothing }
      CommitmentQueryTuple{..} -> q { comQuery'commitmentTuple = Nothing }

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
    appendComQueries a b = case (a, b) of
      (CommitmentQueryLog inpA comA, CommitmentQueryLog inpB comB)     | inpA == inpB -> Just $ a { comQuery'commitmentLog   = comA <|> comB }
      (CommitmentQueryTuple inpA comA, CommitmentQueryTuple inpB comB) | inpA == inpB -> Just $ a { comQuery'commitmentTuple = comA <|> comB }
      _                                                                               -> Nothing

toCommitmentExpr :: CommitmentQueryExpr a -> Prove (CommitmentExpr a)
toCommitmentExpr tree = case tree of
  Leaf tag a -> Leaf tag <$> either (fmap Left . toCommitmentResult) (pure . Right) a
  AND tag as -> AND tag  <$> mapM toCommitmentExpr as
  OR  tag as -> OR  tag  <$> mapM toCommitmentExpr as
  where
    toCommitmentResult = \case
      CommitmentQueryLog{..}   -> maybe noCommitmentError (\com -> pure $ CommitmentResultLog comQuery'publicLog com) comQuery'commitmentLog
      CommitmentQueryTuple{..} -> maybe noCommitmentError (\com -> pure $ CommitmentResultTuple comQuery'publicTuple com) comQuery'commitmentTuple


    noCommitmentError = throwError "No commitment found"

type ChallengeExpr  a = ProofExpr ChallengeResult a
type CommitmentExpr a = ProofExpr CommitmentResult a

data ChallengeResult a = ChallengeResult
  { challengeResult'result     :: CommitmentResult a
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
        (\case
          CommitmentResultLog{..}   -> FiatShamirLeafDLog comResult'publicLog comResult'commitmentLog
          CommitmentResultTuple{..} -> FiatShamirLeafDTuple comResult'publicTuple comResult'commitmentTuple
        )
        (\case
          ProofDL ProofDLog{..}     -> FiatShamirLeafDLog proofDLog'public proofDLog'commitmentA
          ProofDT ProofDTuple{..}   -> FiatShamirLeafDTuple proofDTuple'public proofDTuple'commitmentA
        )

    withChallenge ch tag = tag { proofTag'challenge = Just ch }

    -- Prover Step 9: complete the proof by computing challenges at real
    -- nodes and additionally responses at real leaves
    goReal ch = \case
      Leaf tag eProof -> case (proofTag'flag tag, eProof) of
        (Real, (Left comRes)) ->
          return $ Leaf (ProofTag Real $ Just ch) $ Left $ ChallengeResult {
                                      challengeResult'result    = comRes
                                    , challengeResult'challenge = ch
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
  { responseQuery'result     :: CommitmentResult a
  , responseQuery'challenge  :: Challenge a
  , responseQuery'response   :: Maybe (Response a)
  }

queryToProofDL :: ResponseQuery a -> Maybe (AtomicProof a)
queryToProofDL ResponseQuery{..} = fmap (\resp -> case responseQuery'result of
  CommitmentResultLog{..} -> ProofDL $ ProofDLog
    { proofDLog'public      = comResult'publicLog
    , proofDLog'commitmentA = comResult'commitmentLog
    , proofDLog'responseZ   = resp
    , proofDLog'challengeE  = responseQuery'challenge
    }
  CommitmentResultTuple{..} -> ProofDT $ ProofDTuple
    { proofDTuple'public      = comResult'publicTuple
    , proofDTuple'commitmentA = comResult'commitmentTuple
    , proofDTuple'responseZ   = resp
    , proofDTuple'challengeE  = responseQuery'challenge
    }
  ) responseQuery'response


type ResponseQueryExpr a = ProofExpr ResponseQuery a

queryResponses :: EC a => Env a -> CommitmentSecretExpr a -> ChallengeExpr a -> Prove (ResponseQueryExpr a)
queryResponses env secretExpr expr = case (secretExpr, expr) of
  (Leaf _ (Left secretLeaf), Leaf tag (Left ChallengeResult{..})) -> return $ Leaf tag $ Left $ ResponseQuery
    { responseQuery'result     = challengeResult'result
    , responseQuery'challenge  = challengeResult'challenge
    , responseQuery'response   = fmap (\rand -> toResp challengeResult'challenge rand challengeResult'result) (getSecretRand secretLeaf)
    }
  (Leaf _ _, Leaf tag (Right pdl)) -> return $ Leaf tag $ Right pdl
  (AND _ as, AND tag bs) -> fmap (AND tag) $ zipWithM (queryResponses env) as bs
  (OR  _ as, OR  tag bs) -> fmap (OR  tag) $ zipWithM (queryResponses env) as bs
  _                      -> throwError "Expression trees are different. Failed to generate response"
  where
    getSecretRand CommitmentSecret{..} = comSecret'secret

    toResp challenge rand = getResponseForInput env rand challenge . commitmentResultInput

commitmentResultInput :: CommitmentResult a -> ProofInput a
commitmentResultInput = \case
  CommitmentResultLog   dlog _   -> InputDLog dlog
  CommitmentResultTuple dtuple _ -> InputDTuple dtuple

filterResponses :: EC a => Set (PublicKey a) -> ResponseQueryExpr a -> ResponseQueryExpr a
filterResponses knownKeys = \case
  Leaf tag leaf -> Leaf tag $ first (\query ->
    if ownsResponse query
      then query
      else eraseResponse query
    ) leaf
  AND  tag as -> AND tag $ fmap rec as
  OR   tag as -> OR  tag $ fmap rec as
  where
    rec = filterResponses knownKeys

    eraseResponse q = q { responseQuery'response = Nothing }

    ownsResponse = ownsKey knownKeys . responseQueryInput
    responseQueryInput = commitmentResultInput . responseQuery'result

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
           responseQuery'result a     == responseQuery'result b
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


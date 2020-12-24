-- | It defines types and functions for Sigma-expressions.
-- Sigma-expressions are used to sign scripts without providing
-- the information on who signed the script.
--
-- It is the way to prove ownership of the private-key.
module Hschain.Utxo.Lang.Sigma(
    CryptoAlg
  , KeyPair
  , PublicKey
  , ECPoint
  , Secret
  , ProofEnv
  , ProofInput
  , Proof
  , AtomicProof
  , SigMessage(..)
  , Sigma
  , sigmaPk
  , dlogSigma
  , dtupleSigma
  , mapPk
  , mapPkM
  , SigmaF(..)
  , newProof
  , verifyProof
  , publicKeyFromText
  , publicKeyToText
  , emptyProofEnv
  , proofEnvFromKeys
  , newSecret
  , getPublicKey
  , getKeyPair
  , toProofEnv
  , equalSigmaExpr
  , equalSigmaProof
  , eliminateSigmaBool
  -- * Multi-signatures
  , Prove
  , runProve
  , initMultiSigProof
  , queryCommitments
  , appendCommitments
  , getChallenges
  , queryResponses
  , appendResponsesToProof
  , checkChallenges
  , dlogInput
  , dtupleInput
  ) where

import Hex.Common.Serialise

import Control.Monad.Except
import Control.DeepSeq (NFData,NFData1)
import Codec.Serialise

import Data.Aeson
import Data.ByteString (ByteString)
import Data.Boolean
import Data.Bifunctor
import Data.Data
import Data.Default
import Data.Either
import Data.Fix
import Data.Functor.Classes (Eq1(..))
import Data.Maybe
import Data.Set (Set)
import Data.Text (Text)
import Data.Eq.Deriving
import Data.Ord.Deriving

import GHC.Generics (Generic, Generic1)

import Text.Show.Deriving

import HSChain.Crypto.Classes      (ViaBase58(..), ByteRepr(..))
import HSChain.Crypto.Classes.Hash
import HSChain.Crypto.SHA          (SHA256)
import qualified HSChain.Crypto as Crypto
import qualified Hschain.Utxo.Lang.Sigma.Interpreter           as Sigma
import qualified Hschain.Utxo.Lang.Sigma.EllipticCurve         as Sigma
import qualified Hschain.Utxo.Lang.Sigma.MultiSig              as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Protocol              as Sigma
import qualified Hschain.Utxo.Lang.Sigma.DTuple                as Sigma
import qualified Hschain.Utxo.Lang.Sigma.Types                 as Sigma
import Hschain.Utxo.Lang.Sigma.Interpreter (Prove, runProve)


-- | Cryptographic algorithm that we use.
type CryptoAlg = Sigma.Ed25519

-- | Pair of public and private keys.
type KeyPair    = Sigma.KeyPair    CryptoAlg

-- | Public key.
type PublicKey  = Crypto.PublicKey  CryptoAlg

-- | Private key.
type Secret     = Crypto.PrivKey    CryptoAlg

-- | Environment to prove the ownership. It is a list of key-pairs
-- that are owned by the prover.
type ProofEnv   = Sigma.Env        CryptoAlg

type ECPoint    = Sigma.ECPoint    CryptoAlg

-- | Proof of the ownership.
type ProofInput  = Sigma.ProofInput   CryptoAlg
type Proof       = Sigma.Proof        CryptoAlg
type AtomicProof = Sigma.AtomicProof  CryptoAlg

-- | Message for signature it is computed based on Tx.
newtype SigMessage = SigMessage (Hash SHA256)
  deriving newtype  (Show, Eq, Ord, NFData, ByteRepr, CryptoHashable)
  deriving stock    (Generic)
  deriving anyclass (Serialise)
  deriving (ToJSON, FromJSON, ToJSONKey, FromJSONKey) via (ViaBase58 "SigMessage" ByteString)

instance Default SigMessage where
  def = fromJust $ decodeFromBS ""

-- | Generate new private key.
newSecret :: IO Secret
newSecret = Crypto.generatePrivKey

-- | Convert private key to public key.
getPublicKey :: Secret -> PublicKey
getPublicKey = Crypto.publicKey

-- | Creates key-pair for given private key.
getKeyPair :: Secret -> KeyPair
getKeyPair secret = Sigma.KeyPair secret (getPublicKey secret)

-- | Proof environment is a listavailable key-pairs.
toProofEnv :: [KeyPair] -> ProofEnv
toProofEnv ks = Sigma.Env ks

-- | Parse public key from text.
publicKeyFromText :: Text -> Maybe PublicKey
publicKeyFromText = serialiseFromText

-- | Convert public key to text.
publicKeyToText :: PublicKey -> Text
publicKeyToText = serialiseToText

instance Serialise a => ToJSON (Sigma a) where
  toJSON = serialiseToJSON

instance Serialise a => FromJSON (Sigma a) where
  parseJSON = serialiseFromJSON

-- | Creates proof for sigma expression with given collection of key-pairs (@ProofEnv@).
-- The last argument message is a serialised content of transaction.
-- It's message to be signed.
--
-- For the message use getTxBytes from TX that has no proof.
newProof :: ProofEnv -> Sigma ProofInput -> SigMessage -> IO (Either Text Proof)
newProof env expr message =
  case toSigmaExprOrFail expr of
    Right sigma -> Sigma.newProof env sigma $ encodeToBS message
    Left  err   -> return $ Left err

-- | Verify the proof.
--
-- > verifyProof proof message
--
-- For the message use getTxBytes from TX.
verifyProof :: Proof -> SigMessage -> Bool
verifyProof proof = Sigma.verifyProof proof . encodeToBS

type Sigma k = Fix (SigmaF k)

mapPk :: (a -> b) -> Sigma a -> Sigma b
mapPk f = foldFix $ \case
  SigmaPk a   -> Fix $ SigmaPk (f a)
  SigmaAnd as -> Fix $ SigmaAnd as
  SigmaOr  as -> Fix $ SigmaOr  as
  SigmaBool b -> Fix $ SigmaBool b

mapPkM :: Monad m => (a -> m b) -> Sigma a -> m (Sigma b)
mapPkM f = foldFixM $ \case
  SigmaPk a   -> fmap (Fix . SigmaPk) (f a)
  SigmaAnd as -> pure $ Fix $ SigmaAnd as
  SigmaOr  as -> pure $ Fix $ SigmaOr  as
  SigmaBool b -> pure $ Fix $ SigmaBool b

instance Boolean (Sigma k) where
  true  = Fix $ SigmaBool True
  false = Fix $ SigmaBool False

  notB  = error "Not is not defined for Sigma-expressions"
  (&&*) a b = Fix $ SigmaAnd [a, b]
  (||*) a b = Fix $ SigmaOr [a, b]

sigmaPk :: k -> Sigma k
sigmaPk k = Fix $ SigmaPk k

dlogSigma :: PublicKey -> Sigma ProofInput
dlogSigma k = sigmaPk $ dlogInput k

dtupleSigma :: ECPoint -> PublicKey -> PublicKey -> Sigma ProofInput
dtupleSigma genB keyA keyB = sigmaPk $ dtupleInput genB keyA keyB

-- | Sigma-expression
data SigmaF k a =
    SigmaPk k      -- ownership of the key (contains public key)
  | SigmaAnd [a]   -- and-expression
  | SigmaOr  [a]   -- or-expression
  | SigmaBool Bool -- wraps boolean constants
  deriving stock (Functor, Foldable, Traversable, Show, Read, Eq, Ord, Generic, Generic1, Data)
  deriving anyclass (NFData, NFData1, Serialise)

instance Serialise k => Serialise (Fix (SigmaF k))

instance (CryptoHashable k, CryptoHashable a) => CryptoHashable (SigmaF k a) where
  hashStep = genericHashStep Sigma.hashDomain

fromSigmaExpr :: Sigma.SigmaE () a -> Sigma a
fromSigmaExpr = \case
  Sigma.Leaf _ k -> Fix $ SigmaPk k
  Sigma.AND _ as -> Fix $ SigmaAnd $ fmap rec as
  Sigma.OR _ as  -> Fix $ SigmaOr  $ fmap rec as
  where
    rec  = fromSigmaExpr

-- | Tries to remove all boolean constants.
-- returns Left boolean if it's not possible
-- to eliminate boolean constants.
eliminateSigmaBool :: Sigma a -> Either Bool (Sigma a)
eliminateSigmaBool = foldFix $ \case
  SigmaBool b -> Left b
  SigmaPk pk  -> Right $ Fix $ SigmaPk pk
  SigmaAnd as ->
    let (bools, sigmas) = partitionEithers as
        boolRes = and bools
    in  if boolRes
          then
              case sigmas of
                []      -> Left True
                [sigma] -> Right sigma
                _       -> Right $ Fix $ SigmaAnd sigmas
          else Left False
  SigmaOr as ->
    let (bools, sigmas) = partitionEithers as
        boolRes = or bools
    in  if boolRes
          then Left True
          else
               case sigmas of
                 []      -> Left False
                 [sigma] -> Right sigma
                 _       -> Right $ Fix $ SigmaOr sigmas

toSigmaExpr :: Sigma a -> Either Bool (Sigma.SigmaE () a)
toSigmaExpr a = (maybe (Left False) Right . toPrimSigmaExpr) =<< eliminateSigmaBool a

toSigmaExprOrFail :: Sigma a -> Either Text (Sigma.SigmaE () a)
toSigmaExprOrFail a = bimap catchBoolean id $ toSigmaExpr a
  where
    catchBoolean = const "Expression is constant boolean. It is not  a sigma-expression"

toPrimSigmaExpr :: Sigma a -> Maybe (Sigma.SigmaE () a)
toPrimSigmaExpr = foldFix $ \case
  SigmaPk k    -> Just $ Sigma.Leaf () k
  SigmaAnd as  -> fmap (Sigma.AND ()) $ sequence as
  SigmaOr  as  -> fmap (Sigma.OR  ()) $ sequence as
  SigmaBool _  -> Nothing

-- | Empty proof environment. It holds no keys.
emptyProofEnv :: ProofEnv
emptyProofEnv = Sigma.Env []

-- | Wrapper to contruct proof environment from list of key-pairs.
proofEnvFromKeys :: [KeyPair] -> ProofEnv
proofEnvFromKeys = Sigma.Env

-- | Check if sigma expression is proven with given proof.
equalSigmaProof :: Sigma ProofInput -> Proof -> Bool
equalSigmaProof candidate proof =
  equalSigmaExpr
      candidate
      (fromSigmaExpr $ Sigma.completeProvenTree proof)

equalSigmaExpr :: Sigma ProofInput -> Sigma AtomicProof -> Bool
equalSigmaExpr (Fix x) (Fix y) = case (x, y) of
  (SigmaPk inp, SigmaPk proof)     -> inp == Sigma.getProofInput proof
  (SigmaOr as, SigmaOr bs)         -> equalList as bs
  (SigmaAnd as, SigmaAnd bs)       -> equalList as bs
  _                                -> False
  where
    equalList = liftEq equalSigmaExpr

----------------------------------------------------------------------------
-- Multi signatures
--
-- Tools to create cooperative signature with multiple partners.
--
-- It takes several steps to complete the proof. We should assign main prover.
-- Main prover carries the main steps of the proof and asks participants for missing info.
-- As in case of any sigma-protocol there are three key elements: commitments, challenges, responses.
--
-- In summary algorithm flows along these steps:
--  * main prover creates simulated proofs and asks for commitments for real proofs
--  * participants share commitments and keep corresponfing secret private
--  * based on commitments main prover calculates challenges and asks for responces
--  * participants provide responses derived from their private keys and secrets generated on the commitment-stage
--  * main prover completes the proof when he gets all responses.
--
-- For example we have three participants Alice, Bob and John.
-- They want to sign the TX with sigma-expression:
--
-- > pkAlice && pkBob && pkJohn
--
-- At first stage we have to choose a main prover. Main prover carries algorithm and
-- creates the proof and querries other participants along the way.
-- Let's set Alice as a main prover.
--
-- Alice calls the function @initMultiSigProof@ to start the proof creation.
-- Everything happens in the Prove monad which is instance of MonadIO and we can query participants through IO-actions.
--
-- > participantKeys = Set.fromList [alicePubKey, bobPubKey, johnPubKey]
-- >
-- > queryCommitmentsExpr <- initMultiSigProof participantKeys expr
--
-- We get expression @queryCommitmentsExpr@ that comtains simulated proofs for fake nodes and
-- holes to fill by participants for real commitments. Commitment is derived from random secret that
-- every participant should keep  private and pass commitment to main prover.
--
-- We create commitments and secrets with the function @queryCommitments@
--
-- > (aliceCommitments, aliceSecrets) <- queryCommitments [alicePubKey] queryCommitmentsExpr
-- > (bobCommitments, bobSecrets)     <- queryCommitments [bobPubKey]   queryCommitmentsExpr
-- > (johnCommitments, johnSecrets)   <- queryCommitments [johnPubKey]  queryCommitmentsExpr
--
-- Every participant generates commitment and secret and gaves commitment to main prover (Alice).
-- Alice gets all commitments and appends them together to create challenges with given message to sign:
--
-- > commitments <- appendCommitments [([alicePubKey], aliceCommitments), ([bobPubKey], bobCommitments), ([johnPubKey], johnCommitments) ] message
-- > challenges <- getChallenges commitments message
--
-- Next main prover asks all participants for responces. Prover gives expression of challenges
-- and each participant uses his own private keys and commitment secrets to calculate responces:
--
-- > aliceResponses <- queryResponses alicePrivateKeys aliceSecrets challenges
-- > bobResponses   <- queryResponses bobPrivateKeys   bobSecrets   challenges
-- > johnResponses  <- queryResponses johnPrivateKeys  johnSecrets  challenges
--
-- Participants give responses to main prover and she creates the proof with function @appendResponsesToProof@:
--
-- > proof <- appendResponsesToProof [([alicePubKey], aliceResponses), ([bobKeys], bobResponses), ([johnKeys], johnResponses)]
--
-- Everything happens in the Prove monad now we can get the result with @runProve@.
-- We can carry dialog with participants over IO, since prove has instance of class @MonadIO@.
-- The routine of main prover can look like this:
--
-- > proof <- runProve $ do
-- >   queryCommitmentsExpr <- initMultiSigProof allKeys
-- >   let partners = [aliceKeys, bobKeys, johnKeys]
-- >   commitments <- mapM askForCommitments partners
-- >   challenges  <- appendCommitments $ zip partners commitments
-- >   responses   <- mapM askForResponses partners
-- >   appendResponsesToProof responses
--
-- Other participants carry on the dialog. They are asked for commitments. They create commitments and secrets
-- and second time they are asked for responses and they use their own private keys and secrets from the first
-- round of the dialog to construct the responses.
--
-- Also participants can check the challenges to be sure that main prover signs correct message that they expect
-- and not resuses their commitments and responses to sign malicious transactions.
-- We can do it with checkChallenges function:
--
-- > checkChallenges commitments challenges message
--
-- It expects the complete list of commitments from all participants (result of the function @appendCommitments@),
-- challenges and the message to be signed and returns boolean.

type QueryCommitments  = Sigma.CommitmentQueryExpr  CryptoAlg
type CommitmentSecrets = Sigma.CommitmentSecretExpr CryptoAlg
type Commitments       = Sigma.CommitmentExpr       CryptoAlg
type Challenges        = Sigma.ChallengeExpr        CryptoAlg
type QueryResponses    = Sigma.ResponseQueryExpr    CryptoAlg

-- | Inits multi-sig proof. Marks tree nodes as simulated and real proofs based
-- on set of known public keys of the group of partners who sign the message.
-- It creates value to query commitments.
initMultiSigProof :: Set PublicKey -> Sigma ProofInput -> Prove QueryCommitments
initMultiSigProof knownKeys expr =
  case toSigmaExprOrFail expr of
    Right sigma -> Sigma.initMultiSigProof knownKeys sigma
    Left err    -> throwError err

-- | Every partner creates a commitment of random secret based on his public keys.
-- The result of the function is a pair of public commitmnets that are handed to the main prover
-- and private secrets that nobody should know. We should use secrets on the latted stages of the algorithm.
queryCommitments :: Set PublicKey -> QueryCommitments -> Prove (QueryCommitments, CommitmentSecrets)
queryCommitments = Sigma.queryCommitments

-- | Appends commitments queried from allpartners.
appendCommitments :: [(Set PublicKey, QueryCommitments)] -> Prove Commitments
appendCommitments = Sigma.appendCommitments

-- | Creates challenges for the given set of commitments.
getChallenges :: Commitments -> SigMessage -> Prove Challenges
getChallenges commitments message =
  liftEither $ Sigma.getChallenges commitments (encodeToBS message)

-- | Query responses. Notice that here we need to supply private keys and commitment secrets
-- that we keep private from the stage of @queryCommitments@.
queryResponses :: ProofEnv -> CommitmentSecrets -> Challenges -> Prove QueryResponses
queryResponses = Sigma.queryResponses

-- | Completes the proof by appending responses from all participants.
appendResponsesToProof :: [(Set PublicKey, QueryResponses)] -> Prove Proof
appendResponsesToProof = Sigma.appendResponsesToProof

-- | Participants of the multisignature can check that the main prover signs correct message.
-- First argument is the result of @appendCommitments@.
checkChallenges :: Commitments -> Challenges -> SigMessage -> Bool
checkChallenges commitments challenges message =
  Sigma.checkChallenges commitments challenges (encodeToBS message)

dlogInput :: PublicKey -> ProofInput
dlogInput = Sigma.InputDLog

dtupleInput :: ECPoint -> PublicKey -> PublicKey -> ProofInput
dtupleInput genB keyA keyB =
  Sigma.InputDTuple $ Sigma.DTuple Sigma.groupGenerator genB keyA keyB

$(deriveShow1 ''SigmaF)
$(deriveEq1   ''SigmaF)
$(deriveOrd1  ''SigmaF)

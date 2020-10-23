{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingVia         #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}
import Control.Monad
import Control.Exception

import Data.Int
import Data.Fix
import Data.Foldable
import Data.Text (Text)
import Data.Map.Strict (Map,(!))
import Data.Yaml (decodeFileThrow)
import qualified Data.Sequence   as Seq
import qualified Data.Vector     as V

import Options.Applicative
import Servant.Client
import Servant.Client.Generic
import Servant.API.Generic    (fromServant)
import Network.HTTP.Client    (newManager, defaultManagerSettings)
import Text.Printf
import GHC.Generics (Generic)

import HSChain.Crypto
import HSChain.PoW.API

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Pow.App
import Hschain.Utxo.Pow.App.Types

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

main :: IO ()
main = do
  join $ customExecParser (prefs showHelpOnError)
       $ info (helper <*> parser)
         (  fullDesc
         <> header   "Program for working with keys for coin node"
         <> progDesc ""
         )

parser :: Parser (IO ())
parser = subparser $ mconcat
  [ command "balance" (parseBalance `info` header "Print balances of different users")
  , command "send"    (parseSend    `info` header "Send coins to given user")
  ]

----------------------------------------------------------------
-- Request balance from node
----------------------------------------------------------------

optionManager :: Parser (IO ClientEnv)
optionManager = do
  pure $ do
    mngr <- newManager defaultManagerSettings
    pure $ mkClientEnv mngr (BaseUrl Http "localhost" 8080 "")
  

parseBalance :: Parser (IO ())
parseBalance = do
  mkManager <- optionManager
  pure $ do
    env   <- mkManager
    boxes <- debugGetState (nodeRoutes env)
    -- Compute balances
    let balances = foldMap toBalance boxes
    printf "Public = %i\n" (boxSetSum $ publicBox balances)

parseSend :: Parser (IO ())
parseSend = do
  mkEnv  <- optionManager
  key    <- strArgument (help "User name")
  amount <- argument auto (help "Amount to send")
  pure $ do
    keymap :: Map Text Text <- decodeFileThrow "keyring.yaml"
    let Just (sk :: Secret) = decodeBase58 $ keymap ! key
        pk = getPublicKey sk
    --
    env   <- mkEnv
    boxes <- debugGetState (nodeRoutes env)
    --
    let select _ []     = []
        select n ((bid,b):bs)
          | n' >= amount = [bid]
          | otherwise    = bid : select n' bs
          where n' = n + box'value b
        inputs = select 0 $ toList $ boxSetBoxes $ publicBox $ foldMap toBalance boxes
    -- Prepare transaction
    let tx :: Tx
        tx = Tx
          { tx'inputs  = V.fromList [ BoxInputRef bid mempty Nothing V.empty SigAll
                                    | bid <- inputs
                                    ]
          , tx'outputs = V.fromList [
              Box { box'value  = amount
                  , box'script = coreProgToScript $ EPrim $ PrimSigma $ Fix $ SigmaPk pk
                  , box'args   = mempty
                  }
              ]
          }
    print =<< runClientM (mempoolPostTxSync callMempoolRestAPI tx) env
    return ()


toBalance :: (BoxId, Box) -> Balance
toBalance pair@(_boxId, Box{..})
  | script == EPrim (PrimBool True) = mempty { publicBox = boxset }
  | otherwise                       = mempty
  where
    Just script = coreProgFromScript box'script
    boxset      = BoxSet box'value (Seq.singleton pair)

  


data BoxSet = BoxSet
  { boxSetSum   :: !Int64 
  , boxSetBoxes :: Seq.Seq (BoxId,Box)
  }
  deriving stock (Show, Generic)

data Balance = Balance
  { publicBox :: BoxSet
  }
  deriving stock (Show, Generic)

instance Semigroup BoxSet where
  BoxSet n1 s1 <> BoxSet n2 s2 = BoxSet (n1 + n2) (s1 <> s2)

instance Monoid BoxSet where
  mempty = BoxSet 0 mempty

instance Semigroup Balance where
  a <> b = Balance { publicBox = publicBox a <> publicBox b
                   }

instance Monoid Balance where
  mempty = Balance { publicBox = mempty
                   }


----------------------------------------------------------------
-- Client
----------------------------------------------------------------

nodeRoutes :: ClientEnv -> UtxoRestAPI (AsClientT IO)
nodeRoutes env = genericClientHoist
  (\x -> runClientM x env >>= either throwIO return)

-- callEndpointGetBox :: BoxId -> ClientM (Maybe Box)
-- callDebugGetState  :: ClientM [(BoxId,Box)]
callMempoolRestAPI :: MempoolRestAPI UTXOBlock (AsClientT ClientM)
UtxoRestAPI
  { utxoMempoolAPI = (let mempoolTy :: a -> MempoolRestAPI UTXOBlock (AsClientT ClientM)
                          mempoolTy = undefined
                       in fromServant `asTypeOf` mempoolTy) ->
     callMempoolRestAPI 
--  , endpointGetBox = callEndpointGetBox
--  , debugGetState = callDebugGetState
  } = genericClient



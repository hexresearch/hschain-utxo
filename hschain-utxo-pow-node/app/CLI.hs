{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}
import Control.Monad
import Control.Exception
import Options.Applicative
import Servant.Client
import Servant.Client.Generic
import Network.HTTP.Client    (newManager, defaultManagerSettings)
import Data.Map.Strict

import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Pow.App

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
  [ command "balance" (parseBalance `info` header "Generate key(s)")
  ]

----------------------------------------------------------------
-- Request balance from node
----------------------------------------------------------------

parseBalance :: Parser (IO ())
parseBalance = do
  pure $ do
    mngr <- newManager defaultManagerSettings
    let env = mkClientEnv mngr (BaseUrl Http "localhost" 8080 "")
    boxes <- debugGetState (nodeRoutes env)
    -- Compute balances
    forM_ boxes $ \(boxId, Box{..}) -> do
      print (boxId, box'value, coreProgFromScript box'script)

data Balance = Balance
  {
  }

----------------------------------------------------------------
-- Client
----------------------------------------------------------------

nodeRoutes :: ClientEnv -> UtxoRestAPI (AsClientT IO)
nodeRoutes env = genericClientHoist
  (\x -> runClientM x env >>= either throwIO return)


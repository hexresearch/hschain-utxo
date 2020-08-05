-- | Compiler CLI application.
module Hschain.Utxo.Compiler.App(
  runApp
) where

import Hschain.Utxo.Compiler.Options
import Hschain.Utxo.Compiler.Commands

-- | Run the compiler.
runApp :: IO ()
runApp = app =<< optionParser

app :: Options -> IO ()
app = \case
  Compile{..}       -> compile       compile'input compile'output
  GenPrivateKey{..} -> genPrivateKey genPrivateKey'output
  GetPublicKey{..}  -> getPublicKey  getPublicKey'input getPublicKey'output
  SignSigma{..}     -> signSigma     signSigma'secret signSigma'expression signSigma'tx signSigma'output


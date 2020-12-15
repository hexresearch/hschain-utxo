import Data.Yaml.Config
import System.Environment
import Hschain.Utxo.Pow.App

main :: IO ()
main = do
  args   <- getArgs
  config <- loadYamlSettings args [] requireEnv
  runLightNode genesisMock config

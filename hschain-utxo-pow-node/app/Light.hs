import Data.Yaml.Config
import System.Environment
import System.IO
import Hschain.Utxo.Pow.App

main :: IO ()
main = do
  hSetBuffering stderr LineBuffering
  hSetBuffering stdout LineBuffering
  args   <- getArgs
  config <- loadYamlSettings args [] requireEnv
  runLightNode genesisMock config

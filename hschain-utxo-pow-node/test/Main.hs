
import Test.Tasty
import qualified TM.Store
import qualified TM.Blocks
import qualified TM.SmartCon.XorGame

main :: IO ()
main = defaultMain $ testGroup "pow-node"
  [ TM.Store.tests
  , TM.Blocks.tests
  , TM.SmartCon.XorGame.tests
  ]


import Test.Tasty
import qualified TM.Store
import qualified TM.Blocks

main :: IO ()
main = defaultMain $ testGroup "pow-node"
  [ TM.Store.tests
  , TM.Blocks.tests
  ]

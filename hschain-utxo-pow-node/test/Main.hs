
import Test.Tasty
import qualified TM.Store

main :: IO ()
main = defaultMain $ testGroup "pow-node"
  [ TM.Store.tests
  ]

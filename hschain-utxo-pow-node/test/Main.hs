
import Test.Tasty
import qualified TM.Store
import qualified TM.SmartCon.Basic
import qualified TM.SmartCon.XorGame
import qualified TM.SmartCon.PayForCoffee

main :: IO ()
main = defaultMain $ testGroup "pow-node"
  [ TM.Store.tests
  , TM.SmartCon.Basic.tests
  , TM.SmartCon.PayForCoffee.tests
  , TM.SmartCon.XorGame.tests
  ]

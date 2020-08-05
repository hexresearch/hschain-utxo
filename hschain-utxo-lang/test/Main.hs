import Test.Tasty
import qualified TM.Core
import qualified TM.Core.Box
import qualified TM.Core.List
import qualified TM.Tx.Sigma

main :: IO ()
main = defaultMain $ testGroup "lang"
  [ TM.Core.tests
  , TM.Core.Box.tests
  , TM.Core.List.tests
  , TM.Tx.Sigma.tests
  ]

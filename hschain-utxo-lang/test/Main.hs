import Test.Tasty
import qualified TM.Core
import qualified TM.Core.Box
import qualified TM.Core.List

main :: IO ()
main = defaultMain $ testGroup "lang"
  [ TM.Core.tests
  , TM.Core.Box.tests
  , TM.Core.List.tests
  ]

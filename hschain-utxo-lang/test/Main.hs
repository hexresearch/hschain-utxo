import Test.Tasty
import qualified TM.Core
import qualified TM.Core.Box
-- import qualified TM.Core.Bytes
import qualified TM.Core.Cost
import qualified TM.Core.List
import qualified TM.Tx.Sigma

main :: IO ()
main = defaultMain $ testGroup "lang"
  [ TM.Core.tests
  , TM.Core.Box.tests
-- todo: bytes test do not work!
--  , TM.Core.Bytes.tests
  , TM.Core.Cost.tests
  , TM.Core.List.tests
  , TM.Tx.Sigma.tests
  ]

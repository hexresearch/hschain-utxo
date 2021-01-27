import Test.Tasty
import qualified TM.Core
import qualified TM.Core.Box
import qualified TM.Core.Bytes
import qualified TM.Core.Int
import qualified TM.Core.List
import qualified TM.Lang.Scripts
import qualified TM.Lang.UserTypes
import qualified TM.Tx.DTuple
import qualified TM.Tx.Sigma
import qualified TM.Tx.Sign

main :: IO ()
main = defaultMain $ testGroup "lang"
  [ TM.Core.tests
  , TM.Core.Box.tests
  , TM.Core.Bytes.tests
  , TM.Core.Int.tests
  , TM.Core.List.tests
  , TM.Lang.Scripts.tests
  , TM.Lang.UserTypes.tests
  , TM.Tx.DTuple.tests
  , TM.Tx.Sigma.tests
  , TM.Tx.Sign.tests
  ]

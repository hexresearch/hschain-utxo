import Test.Tasty
import qualified TM.Repl

main :: IO ()
main = defaultMain $ testGroup "repl"
  [ TM.Repl.tests
  ]


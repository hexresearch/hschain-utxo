-- | Test repl execution
module TM.Repl(
  tests
)where

import Control.Monad.State.Strict

import Test.Tasty
import Test.Tasty.HUnit
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Repl(eval, defaultTxArg)
import Hschain.Utxo.Repl.Monad
import Data.Text (Text)

tests :: TestTree
tests = testGroup "repl"
  [ testReplOk   "numeric ops" numOps
  , testReplOk   "text ops" textOps
  , testReplOk   "boolean ops"  boolOps
  , testReplOk   "sigma ops"  sigmaOps
  , testReplOk   "bytestring literals" bytesLiterals
  , testReplOk   "LHS patterns" lhsPatBinds
  , testReplOk   "User defined data deckaration" dataDecls
  , testReplFail "add num and string" addNumAndText
  , testReplFail "var not in scope" varNotInScope
  , testReplFail "type is not defined" typeIsNotDefined
  , testReplFail "wrong kinds" wrongKinds1
  , testReplFail "wrong kinds" wrongKinds2
  , testReplOk   "unit as argument of constructor" unitConsArg
  ]
  where
    numOps =
      [ "2 + 2"
      , "1 * 3"
      , "q = 10"
      , "w = 20"
      , "q - w"
      ]

    addNumAndText =
      [ "q = 2 + \"foo\""
      ]

    varNotInScope =
      [ "a = 1"
      , "a + b"
      ]

    textOps =
      [ "q = \"Hello \""
      , "w = \"World!\""
      , "q <> w"
      , "lengthText q"
      ]

    boolOps =
      [ "True && False"
      , "True || True"
      , "not False"
      ]

    sigmaOps =
      [ "toSigma True &&* toSigma False"
      , "toSigma True ||* toSigma False"
      ]

    bytesLiterals =
      [ "q = bytes \"asdf\""
      , "sha256 q"
      , "lengthBytes q"
      ]

    lhsPatBinds =
      [ "q = (1, 2)"
      , "(r, t) = q"
      , "r + t"
      ]

    dataDecls =
      [ "data Color = Rgb Int Int Int"
      , "data Q = Q Color | A"
      , ":t Rgb"
      , "f x = Rgb 1 2 x"
      , "Q (f 1)"
      ]

    typeIsNotDefined =
      [ "data Color = Rgb Int Int Intt"
      ]

    wrongKinds1 =
      [ "data T = A | B (Maybe Int Text)"
      ]

    wrongKinds2 =
      [ "data T = A | B (Int Text)"
      ]

    unitConsArg =
      [ "data F = F () Int"
      , "q = F () 10"
      ]

testReplOk :: String -> [String] -> TestTree
testReplOk title commands = testRepl title True commands

testReplFail :: String -> [String] -> TestTree
testReplFail title commands = testRepl title False commands

testRepl :: String -> Bool -> [String] -> TestTree
testRepl title answer commands = case answer of
  True  -> testCase title $ ([] @=? ) =<< checkRepl commands
  False -> testCase title $ ((False @=? ) . null) =<< checkRepl commands

-- | Runs REPL on list of commands and returns list of error-messages
-- If everything runs ok then list is empty.
checkRepl :: [String] -> IO [Text]
checkRepl commands = runRepl defaultTxArg $ do
  mapM_ eval commands
  fmap (fmap renderText . replEnv'errors) get


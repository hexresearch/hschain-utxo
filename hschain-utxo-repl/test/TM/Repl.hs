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
  , testReplFail "add num and string" addNumAndText
  , testReplFail "var not in scope" varNotInScope
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
      [ "sigmaAnd (toSigma True) (toSigma False)"
      , "sigmaOr (toSigma True) (toSigma False)"
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


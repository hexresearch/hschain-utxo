-- |
module TM.Core.Common
  ( env
  , testProgram
  , testProgramL
  , testProgramFail
  , mkBoxInput
  , mkBoxOutput
  ) where

import Data.Default
import Data.Int

import Test.Tasty
import Test.Tasty.HUnit
import HSChain.Crypto (hash)
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Core.RefEval
import Hschain.Utxo.Lang.Core.Compile.Expr
import Hschain.Utxo.Lang.Core.Compile.TypeCheck
import Hschain.Utxo.Lang.Core.Types

import qualified Data.Vector as V

env :: InputEnv
env = InputEnv
  { inputEnv'height   = 123
  , inputEnv'self     = mkBoxInput (BoxId $ hash ()) Box
      { box'value  = 100
      , box'script = Script ""
      , box'args   = mempty
      }
  , inputEnv'inputs     = mempty
  , inputEnv'outputs    = mempty
  , inputEnv'dataInputs = mempty
  }

mkBoxInput :: BoxId -> Box -> BoxInput
mkBoxInput bid box = BoxInput
  { boxInput'id      = bid
  , boxInput'box     = PostBox
                          { postBox'content = box
                          , postBox'height  = 1
                          }
  , boxInput'args    = mempty
  , boxInput'proof   = Nothing
  , boxInput'sigs    = mempty
  , boxInput'sigMask = SigAll
  , boxInput'sigMsg  = SigMessage (hash ())
  }

mkBoxOutput :: Int64 -> BoxId -> Box -> BoxOutput
mkBoxOutput height bid box = BoxOutput
  { boxOutput'id  = bid
  , boxOutput'box = PostBox
                        { postBox'content = box
                        , postBox'height  = height
                        }
  }

testProgram :: String -> Core Name -> Prim -> TestTree
testProgram nm prog r = testProgramBy nm prog (Right $ PrimVal r)

testProgramL :: String -> Core Name -> [Prim] -> TestTree
testProgramL nm prog r = testProgramBy nm prog (Right $ toPrimList r)
  where
    toPrimList = foldr (\a as -> ConVal (ConCons UnitT) (V.fromList [PrimVal a, as])) (ConVal (ConNil UnitT) mempty)

testProgramFail :: String -> Core Name -> TestTree
testProgramFail nm prog = testProgramBy nm prog (Left ())

testProgramBy :: String -> Core Name -> Either e TermVal -> TestTree
testProgramBy nm prog res
  = testGroup nm
  $ testTypecheck
 ++ testEval
  where
    testTypecheck =
      [ testCase "typeCheck" $ case typeCheck prog of
            Left  e -> assertFailure $ show e
            Right _ -> pure ()
      ]
    --
    testEval = case res of
      Left  _   -> []
      Right r   -> [ testCase "simple dB" $ Right r @=? evalProg env prog ]





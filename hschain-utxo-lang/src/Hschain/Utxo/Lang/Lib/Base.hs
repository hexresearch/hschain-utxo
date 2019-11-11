module Hschain.Utxo.Lang.Lib.Base(
    importBase
  , baseNames
) where

import qualified Prelude as P
import Prelude (($))
import Data.Text (Text)

import Data.Fix
import Hschain.Utxo.Lang.Expr

-- | Prelude functions
importBase :: Lang -> Lang
importBase = P.foldl (\f g x -> f (g x)) P.id
  [ all
  , any
  , sum
  , product
  , id
  , const
  , getHeight
  , getSelf
  , getOutput
  , getInput
  , getOutputs
  , getInputs
  , getBoxId
  , getBoxValue
  , getBoxScript
  , getBoxArg
  , sha256
  , blake2b256
  , getVar
  ]

baseNames :: [Text]
baseNames =
  [ "all"
  , "any"
  , "sum"
  , "product"
  , "id"
  , "const"
  , "getHeight"
  , "getSelf"
  , "getOutput"
  , "getInput"
  , "getOutputs"
  , "getInputs"
  , "getBoxId"
  , "getBoxValue"
  , "getBoxScript"
  , "getBoxArg"
  , "sha256"
  , "blake2b256"
  , "getVar"
  ]

all :: Lang -> Lang
all = letIn "all" (Fix (Apply (Fix $ Apply (Fix $ VecE VecFold) f) z))
  where
    f = Fix $ Lam "x" boolTy $ Fix $ Lam "y" boolTy $ Fix $ BinOpE And (Fix $ Var "x") (Fix $ Var "y")
    z = Fix $ PrimE $ PrimBool P.True

any :: Lang -> Lang
any = letIn "any" (Fix (Apply (Fix $ Apply (Fix $ VecE VecFold) f) z))
  where
    f = Fix $ Lam "x" boolTy $ Fix $ Lam "y" boolTy $ Fix $ BinOpE Or (Fix $ Var "x") (Fix $ Var "y")
    z = Fix $ PrimE $ PrimBool P.False

sum :: Lang -> Lang
sum = letIn "sum" (Fix (Apply (Fix $ Apply (Fix $ VecE VecFold) f) z))
  where
    f = Fix $ Lam "x" boolTy $ Fix $ Lam "y" boolTy $ Fix $ BinOpE Plus (Fix $ Var "x") (Fix $ Var "y")
    z = Fix $ PrimE $ PrimInt 0

product :: Lang -> Lang
product = letIn "product" (Fix (Apply (Fix $ Apply (Fix $ VecE VecFold) f) z))
  where
    f = Fix $ Lam "x" boolTy $ Fix $ Lam "y" boolTy $ Fix $ BinOpE Times (Fix $ Var "x") (Fix $ Var "y")
    z = Fix $ PrimE $ PrimInt 1

id :: Lang -> Lang
id = letIn "id" $ Fix $ Lam "x" unTy $ Fix $ Var "x"

const :: Lang -> Lang
const = letIn "const" (Fix $ Lam "x" unTy $ Fix $ Lam "y" unTy $ Fix $ Var "x")

getHeight :: Lang -> Lang
getHeight x = Fix $ Let "getHeight" (Fix $ GetEnv Height) x

getSelf :: Lang -> Lang
getSelf = letIn "getSelf" (Fix $ GetEnv Self)

getOutput :: Lang -> Lang
getOutput = letIn "getOutput" (Fix $ Lam "x" (Fix IntType) $ Fix $ GetEnv $ Output $ Fix $ Var "x")

getInput :: Lang -> Lang
getInput = letIn "getInput" (Fix $ Lam "x" (Fix IntType) $ Fix $ GetEnv $ Input $ Fix $ Var "x")

getOutputs :: Lang -> Lang
getOutputs = letIn "getOutputs" (Fix $ GetEnv $ Outputs)

getInputs :: Lang -> Lang
getInputs = letIn "getInputs" (Fix $ GetEnv $ Inputs)

getBoxId :: Lang -> Lang
getBoxId = letIn "getBoxId" (Fix $ Lam "x" boxTy $ Fix $ BoxE $ BoxAt (Fix $ Var "x") BoxFieldId)

getBoxValue :: Lang -> Lang
getBoxValue = letIn "getBoxValue" (Fix $ Lam "x" boxTy $ Fix $ BoxE $ BoxAt (Fix $ Var "x") BoxFieldValue)

getBoxScript :: Lang -> Lang
getBoxScript = letIn "getBoxScript" (Fix $ Lam "x" boxTy $ Fix $ BoxE $ BoxAt (Fix $ Var "x") BoxFieldScript)

getBoxArg :: Lang -> Lang
getBoxArg = letIn "getBoxArg" (Fix $ Lam "arg" textTy $ Fix $ Lam "box" boxTy $ Fix $ BoxE $ BoxAt (Fix $ Var "box") (BoxFieldArg $ Fix $ Var "arg"))

sha256 :: Lang -> Lang
sha256 = letIn "sha256" (Fix $ Lam "x" textTy $ Fix $ Apply (Fix $ TextE $ TextHash Sha256) (Fix $ Var "x"))

blake2b256 :: Lang -> Lang
blake2b256 = letIn "blake2b256" (Fix $ Lam "x" textTy $ Fix $ Apply (Fix $ TextE $ TextHash Blake2b256) (Fix $ Var "x"))

getVar :: Lang -> Lang
getVar = letIn "getVar" (Fix $ Lam "x" textTy $ Fix $ GetEnv $ GetVar $ Fix $ Var "x")

letIn :: Text -> Lang -> Lang -> Lang
letIn var body x = Fix $ Let var body x

boxTy :: Type
boxTy = Fix BoxType

textTy :: Type
textTy = Fix StringType

unTy :: Type
unTy = Fix UknownType

boolTy :: Type
boolTy = Fix BoolType


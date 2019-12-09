module Hschain.Utxo.Lang.Lib.Base(
    importBase
  , baseNames
) where

import qualified Prelude as P
import Prelude (($))
import Data.Text (Text)

import Type.Type
import Data.Fix
import Hschain.Utxo.Lang.Desugar
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
  , trace
  , lengthText
  , showInt
  , showDouble
  , showMoney
  , showBool
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
  , "trace"
  , "lengthText"
  , "showInt"
  , "showDouble"
  , "showMoney"
  , "showBool"
  ]

all :: Lang -> Lang
all = letIn "all" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc And (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool noLoc P.True

any :: Lang -> Lang
any = letIn "any" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Or (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimBool noLoc P.False

sum :: Lang -> Lang
sum = letIn "sum" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Plus (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt noLoc 0

product :: Lang -> Lang
product = letIn "product" (Fix (Apply noLoc (Fix $ Apply noLoc (Fix $ VecE noLoc (VecFold noLoc)) f) z))
  where
    f = Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ BinOpE noLoc Times (Fix $ Var noLoc "x") (Fix $ Var noLoc "y")
    z = Fix $ PrimE noLoc $ PrimInt noLoc 1

id :: Lang -> Lang
id = letIn "id" $ Fix $ Lam noLoc "x" $ Fix $ Var noLoc "x"

const :: Lang -> Lang
const = letIn "const" (Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ Var noLoc "x")

getHeight :: Lang -> Lang
getHeight x = letIn "getHeight" (Fix $ GetEnv noLoc (Height noLoc)) x

getSelf :: Lang -> Lang
getSelf = letIn "getSelf" (Fix $ GetEnv noLoc (Self noLoc))

getOutput :: Lang -> Lang
getOutput = letIn "getOutput" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ Output noLoc $ Fix $ Var noLoc "x")

getInput :: Lang -> Lang
getInput = letIn "getInput" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ Input noLoc $ Fix $ Var noLoc "x")

getOutputs :: Lang -> Lang
getOutputs = letIn "getOutputs" (Fix $ GetEnv noLoc $ Outputs noLoc)

getInputs :: Lang -> Lang
getInputs = letIn "getInputs" (Fix $ GetEnv noLoc $ Inputs noLoc)

getBoxId :: Lang -> Lang
getBoxId = letIn "getBoxId" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldId)

getBoxValue :: Lang -> Lang
getBoxValue = letIn "getBoxValue" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldValue)

getBoxScript :: Lang -> Lang
getBoxScript = letIn "getBoxScript" (Fix $ Lam noLoc "x" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "x") BoxFieldScript)

getBoxArg :: Lang -> Lang
getBoxArg = letIn "getBoxArg" (Fix $ Lam noLoc "arg" $ Fix $ Lam noLoc "box" $ Fix $ BoxE noLoc $ BoxAt noLoc (Fix $ Var noLoc "box") (BoxFieldArg $ Fix $ Var noLoc "arg"))

sha256 :: Lang -> Lang
sha256 = letIn "sha256" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc $ TextHash noLoc Sha256) (Fix $ Var noLoc "x"))

blake2b256 :: Lang -> Lang
blake2b256 = letIn "blake2b256" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc $ TextHash noLoc Blake2b256) (Fix $ Var noLoc "x"))

getVar :: Lang -> Lang
getVar = letIn "getVar" (Fix $ Lam noLoc "x" $ Fix $ GetEnv noLoc $ GetVar noLoc $ Fix $ Var noLoc "x")

trace :: Lang -> Lang
trace = letIn "trace" (Fix $ Lam noLoc "x" $ Fix $ Lam noLoc "y" $ Fix $ Trace noLoc (Fix $ Var noLoc "x") (Fix $ Var noLoc "y"))

showInt :: Lang -> Lang
showInt = letIn "showInt" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc)) (Fix $ Var noLoc "x"))

showDouble :: Lang -> Lang
showDouble = letIn "showDouble" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc)) (Fix $ Var noLoc "x"))

showBool :: Lang -> Lang
showBool = letIn "showBool" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc)) (Fix $ Var noLoc "x"))

showMoney :: Lang -> Lang
showMoney = letIn "showMoney" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (ConvertToText noLoc)) (Fix $ Var noLoc "x"))

lengthText :: Lang -> Lang
lengthText = letIn "lengthText" (Fix $ Lam noLoc "x" $ Fix $ Apply noLoc (Fix $ TextE noLoc (TextLength noLoc)) (Fix $ Var noLoc "x"))

letIn :: Text -> Lang -> Lang -> Lang
letIn var body x = singleLet noLoc (VarName noLoc var) body x


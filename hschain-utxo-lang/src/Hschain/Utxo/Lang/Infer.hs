module Hschain.Utxo.Lang.Infer where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Fix hiding ((~>))
import Data.Vector (Vector)

import Language.HM (appE, varE, absE, letE, varT, monoT, forAllT, arrowT)

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import Debug.Trace

import qualified Language.HM as H

inferExpr :: Lang -> Either (H.TypeError Loc) Type
inferExpr = H.inferW defaultContext . reduceExpr

reduceExpr :: Lang -> H.Term Loc
reduceExpr (Fix expr) = case expr of
  Var loc var               -> fromVarName var
  Apply loc a b             -> appE loc (rec a) (rec b)
  InfixApply loc a name b   -> fromInfixApply loc (rec a) name (rec b)
  Lam loc var a             -> absE loc (varName'name var) (rec a)
  LamList loc vs a          -> rec $ unfoldLamList loc vs a
  LetRec loc var a b        -> undefined
  PrimE loc prim            -> fromPrim loc prim
  If loc cond t e           -> fromIf loc (rec cond) (rec t) (rec e)
  Pk loc a                  -> fromPk loc (rec a)
  {-
  Let Loc (BindGroup a) a
  Ascr Loc a Type
  -- primitives
  -- logic
  -- tuples
  Tuple Loc (Vector a)
  -- operations
  UnOpE Loc UnOp a
  BinOpE Loc BinOp a a
  -- environment
  GetEnv Loc (EnvId a)
  -- vectors
  VecE Loc (VecExpr a)
  -- text
  TextE Loc (TextExpr a)
  -- boxes
  BoxE Loc (BoxExpr a)
  -- undefined
  Undef Loc
  -- debug
  Trace Loc a a
-}
  where
    rec = reduceExpr

    fromInfixApply loc a name b =
      appE loc (appE (H.getLoc name) (fromVarName name) a) b

    fromVarName VarName{..}  = varE varName'loc varName'name

    fromPrim loc prim = ($ loc) $ case prim of
      PrimInt _    -> intE
      PrimDouble _ -> doubleE
      PrimString _ -> textE
      PrimBool _   -> boolE
      PrimSigma _  -> boolE

    fromIf loc cond t e = app3 loc ifVar cond t e

    fromPk loc a = appE loc (varE loc pkVar) a

    app2 loc var a b = appE loc (appE loc (varE loc var) a) b
    app3 loc var a b c = appE loc (app2 loc var a b) c


defaultContext :: H.Context Loc
defaultContext = H.Context $ M.fromList
  -- primitives
  [ (intVar,    monoT intT)
  , (doubleVar, monoT doubleT)
  , (textVar,   monoT textT)
  , (boolVar,   monoT boolT)
  -- if
  , (ifVar,     forA $ monoT $ boolT `arr` (a `arr` (a `arr` a)))
  -- pk
  , (pkVar,     monoT $ textT `arr` boolT)
  ]
  where
    forA = forAllT noLoc "a"
    a = varT noLoc "a"
    arr = arrowT noLoc

intE, doubleE, textE, boolE :: Loc -> H.Term Loc

intE loc = varE loc intVar
doubleE loc = varE loc doubleVar
textE loc = varE loc textVar
boolE loc = varE loc boolVar

intVar, doubleVar, textVar, boolVar :: H.Var

intVar = "Int"
doubleVar = "Double"
textVar = "Text"
boolVar = "Bool"

ifVar, pkVar :: H.Var

ifVar = "if"
pkVar = "pk"

intT, doubleT :: H.Type Loc

intT = varT noLoc intVar
doubleT = varT noLoc doubleVar



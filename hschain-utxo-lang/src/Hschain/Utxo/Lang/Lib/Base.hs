module Hschain.Utxo.Lang.Lib.Base(
    baseNames
  , baseLibTypeContext
  , baseLibExecCtx
  , baseLibInferCtx
  , baseLibTypes
{-    importBase
  , langTypeContext
  , baseModuleCtx
  , baseFuns
-}
) where

import Prelude ((.), fmap, ($), Monoid(..))
import qualified Prelude as P

import Data.Fix
import Data.Map.Strict (Map)
import Data.Text (Text)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Core.Types (Prim(..))
import Type.Check.HM (monoT, forAllT)

import qualified Hschain.Utxo.Lang.Const as Const
import qualified Data.Map as M
import qualified Type.Check.HM as H
import qualified Data.Vector as V

-- | Defines prelude functions.
-- Note that we only need to define those functions that are combinations of primitive functions.
-- The primitive functions are substituted for primOps on later stage of compilation.
baseLibExecCtx :: ExecCtx
baseLibExecCtx = ExecCtx
  { execCtx'vars = M.fromList $
      [ bind "id"           (lam' ["x"] vx)
      , bind "const"        (lam' ["x", "y"] $ vx)
      , bind "."            (lam' ["f", "g", "x"] (ap1 "f" (ap1 "g" (var' "x"))))
      , bind "getInput"     (lam' ["x"] $ ap2 Const.listAt (var' Const.getInputs) vx)
      , bind "getOutput"    (lam' ["x"] $ ap2 Const.listAt (var' Const.getOutputs) vx)
      , bind "getDataInput" (lam' ["x"] $ ap2 Const.listAt (var' Const.getDataInputs) vx)
      , bind "otherwise"    (Fix $ PrimE noLoc $ PrimBool P.True)
      , bind "fst"          (lamPair $ var' "a")
      , bind "snd"          (lamPair $ var' "b")
      -- maybe funs
      , bind "maybe"        maybeDefn
      , bind "mapMaybe"     mapMaybeDefn
      , bind "fromJust"     fromJustDefn
      , bind "isNothing"    isNothingDefn
      , bind "isJust"       isJustDefn
      ] P.++ sigCmps
  }
  where
    sigCmps = fmap (\(a, b) -> bind a (lam' ["x", "y"] $ toSig $ op2 b vx vy))
      [ (Const.sigmaEquals, Const.equals)
      , (Const.sigmaNonEquals, Const.nonEquals)
      , (Const.sigmaLess, Const.less)
      , (Const.sigmaGreater, Const.greater)
      , (Const.sigmaLessEquals, Const.lessEquals)
      , (Const.sigmaGreaterEquals, Const.greaterEquals)
      ]

    lamPair e = Fix $ Lam noLoc (PTuple noLoc [PVar noLoc "a", PVar noLoc "b"]) e

    toSig = ap1 "toSigma"
    ap1 name e = Fix $ Apply noLoc (var' name) e
    ap2 name a b = Fix $ Apply noLoc (ap1 name a) b
    op2 name a b = Fix $ InfixApply noLoc a (VarName noLoc name) b
    var' name = Fix $ Var noLoc (VarName noLoc name)

    vx = var' "x"
    vy = var' "y"

    lam' names expr = case names of
      []     -> expr
      [name] -> Fix $ Lam noLoc (PVar noLoc $ VarName noLoc name) expr
      _      -> Fix $ LamList noLoc (fmap (PVar noLoc . VarName noLoc) names) expr

    bind var body = (VarName noLoc var, body)

    maybeDefn = lam' ["nothing", "f", "x"] $ Fix $ CaseOf noLoc (var' "x")
      [ CaseExpr
          { caseExpr'lhs = PCons noLoc "Just" [PVar noLoc "a"]
          , caseExpr'rhs = ap1 "f" $ var' "a"
          }
      , CaseExpr
          { caseExpr'lhs = PCons noLoc "Nothing" []
          , caseExpr'rhs = var' "nothing"
          }
      ]

    mapMaybeDefn = lam' ["f", "x"] $ Fix $ CaseOf noLoc (var' "x")
      [ CaseExpr
          { caseExpr'lhs = PCons noLoc "Just" [PVar noLoc "a"]
          , caseExpr'rhs = Fix $ Cons noLoc "Just" $ V.singleton $ ap1 "f" $ var' "a"
          }
      , CaseExpr
          { caseExpr'lhs = PCons noLoc "Nothing" []
          , caseExpr'rhs = Fix $ Cons noLoc "Nothing" V.empty
          }
      ]

    fromJustDefn = lam' ["x"] $ Fix $ CaseOf noLoc (var' "x")
      [ CaseExpr
          { caseExpr'lhs = PCons noLoc "Just" [PVar noLoc "a"]
          , caseExpr'rhs = var' "a"
          }
      ]

    isNothingDefn = lam' ["x"] $ Fix $ CaseOf noLoc (var' "x")
      [ CaseExpr
          { caseExpr'lhs = PCons noLoc "Just" [PVar noLoc "a"]
          , caseExpr'rhs = Fix $ PrimE noLoc $ PrimBool P.False
          }
      , CaseExpr
          { caseExpr'lhs = PCons noLoc "Nothing" []
          , caseExpr'rhs = Fix $ PrimE noLoc $ PrimBool P.True
          }
      ]

    isJustDefn = lam' ["x"] $ Fix $ CaseOf noLoc (var' "x")
      [ CaseExpr
          { caseExpr'lhs = PCons noLoc "Just" [PVar noLoc "a"]
          , caseExpr'rhs = Fix $ PrimE noLoc $ PrimBool P.True
          }
      , CaseExpr
          { caseExpr'lhs = PCons noLoc "Nothing" []
          , caseExpr'rhs = Fix $ PrimE noLoc $ PrimBool P.False
          }
      ]

baseLibInferCtx :: InferCtx
baseLibInferCtx = InferCtx
  { inferCtx'binds = baseLibTypeContext
  , inferCtx'types = setupUserTypeInfo $ mempty { userTypeCtx'types = baseLibTypes }
  }

baseNames :: [Text]
baseNames = ["Just", "Nothing"] P.++ (M.keys $ H.unContext $ baseLibTypeContext)

(~>) :: Type -> Type -> Type
(~>) a b = H.arrowT noLoc a b
infixr 6 ~>

forAllT' :: Text -> Signature -> Signature
forAllT' = forAllT noLoc

assumeType :: Text -> Signature -> (Text, Signature)
assumeType idx ty = (idx, ty)

baseLibTypeContext :: TypeContext
baseLibTypeContext = H.Context $ M.fromList $
  [ assumeType "maybe" (forAB $ bT ~> (aT ~> bT) ~> maybeT aT ~> bT)
  , assumeType "mapMaybe" (forAB $ (aT ~> bT) ~> maybeT aT ~> maybeT bT)
  , assumeType "fromJust" (forA $ maybeT aT ~> aT)
  , assumeType "isNothing" (forA $ maybeT aT ~> boolT)
  , assumeType "isJust" (forA $ maybeT aT ~> boolT)
  , assumeType "and" (monoT $ listT boolT ~> boolT)
  , assumeType "or" (monoT $ listT boolT ~> boolT)
  , assumeType "andSigma" (monoT $ listT sigmaT ~> sigmaT)
  , assumeType "orSigma" (monoT $ listT sigmaT ~> sigmaT)
  , assumeType Const.allSigma (forA $ (aT ~> sigmaT) ~> listT aT ~> sigmaT)
  , assumeType Const.anySigma (forA $ (aT ~> sigmaT) ~> listT aT ~> sigmaT)
  , assumeType Const.all (forA $ (aT ~> boolT) ~> listT aT ~> boolT)
  , assumeType Const.any (forA $ (aT ~> boolT) ~> listT aT ~> boolT)
  , assumeType Const.sum      (monoT $ listT intT ~> intT)
  , assumeType Const.product  (monoT $ listT intT ~> intT)
  , assumeType  "."  (forABC $ (bT ~> cT) ~> (aT ~> bT) ~> (aT ~> cT))
  , assumeType  "id"  (forA $ aT ~> aT)
  , assumeType "const" (forAB $ aT ~> bT ~> aT)
  , assumeType "getHeight" (monoT intT)
  , assumeType "getSelf" (monoT boxT)
  , assumeType "getOutput" (monoT $ intT ~> boxT)
  , assumeType "getInput"  (monoT $ intT ~> boxT)
  , assumeType "getOutputs" (monoT $ listT boxT)
  , assumeType "getInputs" (monoT $ listT boxT)
  , assumeType "getDataInputs" (monoT $ listT boxT)
  , assumeType "getBoxId" (monoT $ boxT ~> textT)
  , assumeType "getBoxValue" (monoT $ boxT ~> intT)
  , assumeType "getBoxPostHeight" (monoT $ boxT ~> intT)
  , assumeType "getBoxScript" (monoT $ boxT ~> bytesT)
  , assumeType "sha256" (monoT $ bytesT ~> bytesT)
  , assumeType "getVar" (forA $ textT ~> aT)
  , assumeType "length" (forA $ listT aT ~> intT)
  , assumeType "lengthText" (monoT $ textT ~> intT)
  , assumeType "lengthBytes" (monoT $ bytesT ~> intT)
  , assumeType "show" (forA $ aT ~> textT)
  , assumeType "not" (monoT $ boolT ~> boolT)
  , assumeType "&&" (monoT $ boolT ~> boolT ~> boolT)
  , assumeType "||" (monoT $ boolT ~> boolT ~> boolT)
  , assumeType Const.sigmaAnd (monoT $ sigmaT ~> sigmaT ~> sigmaT)
  , assumeType Const.sigmaOr (monoT $ sigmaT ~> sigmaT ~> sigmaT)
  , assumeType "pk" (monoT $ bytesT ~> sigmaT)
  , assumeType "dtuple" (monoT $ bytesT ~> bytesT ~> bytesT ~> sigmaT)
  , assumeType "toSigma" (monoT $ boolT ~> sigmaT)
  , assumeType "+" (monoT $ intT ~> intT ~> intT)
  , assumeType "-" (monoT $ intT ~> intT ~> intT)
  , assumeType "negate" (monoT $ intT ~> intT)
  , assumeType "*" (monoT $ intT ~> intT ~> intT)
  , assumeType "/" (monoT $ intT ~> intT ~> intT)
  , assumeType "++" (forA $ listT aT ~> listT aT ~> listT aT)
  , assumeType "<>" (monoT $ textT ~> textT ~> textT)
  , assumeType Const.appendBytes (monoT $ bytesT ~> bytesT ~> bytesT)
  , assumeType Const.filter (forAB $ (aT ~> boolT) ~> listT aT ~> listT aT)
  , assumeType Const.map (forAB $ (aT ~> bT) ~> listT aT ~> listT bT)
  , assumeType Const.foldl (forAB $ (aT ~> bT ~> aT) ~> aT ~> listT bT ~> aT)
  , assumeType Const.foldr (forAB $ (aT ~> bT ~> bT) ~> bT ~> listT aT ~> bT)
  , assumeType Const.length (forA $ listT aT ~> intT)
  , assumeType Const.listAt (forA $ listT aT ~> intT ~> aT)
  , assumeType "==" (forA $ aT ~> aT ~> boolT)
  , assumeType "/=" (forA $ aT ~> aT ~> boolT)
  , assumeType "<" (forA $ aT ~> aT ~> boolT)
  , assumeType "<=" (forA $ aT ~> aT ~> boolT)
  , assumeType ">=" (forA $ aT ~> aT ~> boolT)
  , assumeType ">" (forA $ aT ~> aT ~> boolT)
  , assumeType Const.sigmaEquals (forA $ aT ~> aT ~> sigmaT)
  , assumeType Const.sigmaNonEquals (forA $ aT ~> aT ~> sigmaT)
  , assumeType Const.sigmaLess (forA $ aT ~> aT ~> sigmaT)
  , assumeType Const.sigmaLessEquals (forA $ aT ~> aT ~> sigmaT)
  , assumeType Const.sigmaGreater (forA $ aT ~> aT ~> sigmaT)
  , assumeType Const.sigmaGreaterEquals (forA $ aT ~> aT ~> sigmaT)
  , assumeType "fst" (forAB $ tupleT [aT, bT] ~> aT)
  , assumeType "snd" (forAB $ tupleT [aT, bT] ~> bT)
  , assumeType "otherwise" (monoT boolT)
  , assumeType "undefined" $ forA aT
  , assumeType Const.checkSig $ monoT $ bytesT ~> intT ~> boolT
  , assumeType Const.checkMultiSig $ monoT $ intT ~> listT bytesT ~> listT intT ~> boolT
  , assumeType Const.serialiseBytes $ forA $ aT ~> bytesT
  , assumeType Const.deserialiseBytes $ forA $ bytesT ~> aT
  , assumeType Const.getArgs $ forA $ aT
  , assumeType Const.getBoxArgs $ forA $ boxT ~> aT
  ]
  where
    forA = forAllT' "a" . monoT
    forAB = forAllT' "a" . forAllT' "b" . monoT
    forABC = forAllT' "a" . forAllT' "b" . forAllT' "c" . monoT

    aT, bT, cT :: Type
    aT = varT "a"
    bT = varT "b"
    cT = varT "c"

baseLibTypes :: Map VarName UserType
baseLibTypes = M.fromList
  [("Maybe", maybeType) ]
  where
    maybeType = UserType
      { userType'name  = "Maybe"
      , userType'args  = ["a"]
      , userType'cases = M.fromList
          [ ("Nothing", ConsDef mempty)
          , ("Just",    ConsDef $ V.singleton $ varT "a")
          ]
      }


module Hschain.Utxo.Lang.Desugar.PatternCompiler(

) where

import Language.HM (getLoc)

import Data.Fix
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Exec.Subst

import qualified Data.Vector as V

data PatError
  = NoCasesLeft
  | NoVarFound

type PatM a = Either PatError a

altGroupToExpr :: [Alt Lang] -> Either PatError Lang
altGroupToExpr = runPatM . toCase . toPatternInput

runPatM :: PatM a -> Either PatError a
runPatM = id

toPatternInput :: [Alt Lang] -> Pattern
toPatternInput = undefined

data Pattern = Pattern
  { pattern'args  :: Vector VarName
  , pattern'pats  :: Vector PatCase
  , pattern'other :: Lang
  }

data PatCase = PatCase
  { patCase'lhs :: Vector Pat
  , patCase'rhs :: Lang
  }

toCase :: Pattern -> PatM Lang
toCase pattern@Pattern{..}
  | isEmpty = fromEmpty
  | isVar   = fromVar
  | isCons  = fromCons
  | isMixed = fromMixed
  | otherwise = Left NoCasesLeft
  where
    -- empty rule
    isEmpty = V.null pattern'args

    fromEmpty =
      maybe (Left NoCasesLeft) (Right . patCase'rhs) $ V.headM pattern'pats

    -- var rule
    isVar  = V.all isHeadVar  pattern'pats

    fromVar = (toCase =<<) $ do
      args'   <- maybe (Left NoCasesLeft) Right $ tailM pattern'args
      headVar <- maybe (Left NoCasesLeft) Right $ V.headM pattern'args
      pats'   <- mapM (substVarPat headVar) pattern'pats
      return $ pattern
        { pattern'args = args'
        , pattern'pats = pats'
        }

    -- cons rule
    isCons = V.all isHeadCons pattern'pats

    fromCons = undefined

    -- mixed rule
    isMixed = undefined

    fromMixed = undefined


----------------------------------------------------
-- PatCase

isHeadVar :: PatCase -> Bool
isHeadVar PatCase{..} =
  maybe False isVarPat $ V.headM patCase'lhs

isHeadCons :: PatCase -> Bool
isHeadCons PatCase{..} =
  maybe False isConsPat $ V.headM patCase'lhs

substVarPat :: VarName -> PatCase -> PatM PatCase
substVarPat argVar PatCase{..} = do
  lhs' <- maybe (Left NoCasesLeft) Right $ tailM patCase'lhs
  localVar <- maybe (Left NoVarFound) Right $ getVarPat =<< V.headM patCase'lhs
  let rhs' = subst patCase'rhs localVar (Fix $ Var (getLoc argVar) argVar)
  return $ PatCase lhs' rhs'

----------------------------------------------------
-- utils

tailM :: Vector a -> Maybe (Vector a)
tailM v
  | V.null v  = Nothing
  | otherwise = Just $ V.tail v


isVarPat :: Pat -> Bool
isVarPat = \case
  PVar _ _ -> True
  _        -> False

isConsPat :: Pat -> Bool
isConsPat = \case
  PCons _ _ _ -> True
  PTuple _ _  -> True
  _           -> False

getVarPat :: Pat -> Maybe VarName
getVarPat = undefined



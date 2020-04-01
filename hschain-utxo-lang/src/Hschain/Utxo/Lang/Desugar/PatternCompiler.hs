module Hschain.Utxo.Lang.Desugar.PatternCompiler(
    PatError
  , altGroupToExpr
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Data.Fix
import Data.Function (on)
import Data.Vector (Vector)

import Language.HM (getLoc)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Exec.Subst
import Hschain.Utxo.Lang.Desugar.FreshVar

import qualified Data.List   as L
import qualified Data.Vector as V

-- TODO include locations
data PatError
  = NoCasesLeft
  | NoVarFound
  | NoSameArgsNumber
  | EmptyArgument

type PatM m a = ExceptT PatError m a

altGroupToExpr :: MonadFreshVar m => [Alt Lang] -> m (Either PatError Lang)
altGroupToExpr = runPatM . (toCase <=< toPatternInput)

runPatM :: PatM m a -> m (Either PatError a)
runPatM = runExceptT

toPatternInput :: MonadFreshVar m => [Alt Lang] -> PatM m Pattern
toPatternInput alts
  | checkArgs alts = getPattern
  | otherwise      = throwError NoSameArgsNumber
  where
    checkArgs = sameLength . fmap alt'pats

    sameLength = (== 1) . length . L.nub . fmap length

    getPattern = do
      locs <- getFirstLocs alts
      headVars <- lift $ mapM getFreshVar locs
      return $ Pattern
        { pattern'args  = V.fromList headVars
        , pattern'pats  = V.fromList $ fmap (\Alt{..} -> PatCase (V.fromList alt'pats) alt'expr) alts
        , pattern'other = failCase
        }

    getFirstLocs = \case
      a:_ -> return $ fmap getLoc $ alt'pats a
      []  -> throwError EmptyArgument

    failCase = Fix $ FailCase noLoc

data Pattern = Pattern
  { pattern'args  :: Vector VarName
  , pattern'pats  :: Vector PatCase
  , pattern'other :: Lang
  }

data PatCase = PatCase
  { patCase'lhs :: Vector Pat
  , patCase'rhs :: Lang
  }

toCase :: forall m . Monad m => Pattern -> PatM m Lang
toCase pattern@Pattern{..}
  | isEmpty   = fromEmpty
  | isVar     = fromVar
  | isCons    = fromCons
  | otherwise = fromMixed
  where
    -- empty rule
    isEmpty = V.null pattern'args

    fromEmpty =
      checkNoCases $ fmap patCase'rhs $ V.headM pattern'pats

    -- var rule
    isVar  = V.all isHeadVar  pattern'pats

    fromVar = (toCase =<<) $ do
      args'   <- checkNoCases $ tailM pattern'args
      headVar <- checkNoCases $ V.headM pattern'args
      pats'   <- mapM (substVarPat headVar) pattern'pats
      return $ pattern
        { pattern'args = args'
        , pattern'pats = pats'
        }

    -- cons rule
    isCons = V.all isHeadCons pattern'pats

    fromCons = uncurry toConsCase =<< getConsCases
      where
        toConsCase :: VarName -> [(Pat, Pattern)] -> PatM m Lang
        toConsCase var pats =
          fmap (\xs -> Fix $ CaseOf loc (Fix $ Var loc var) xs) $ mapM toCaseExpr pats
          where
            loc = getLoc var

            toCaseExpr (lhs, rhs) = fmap (CaseExpr lhs) $ toCase rhs

        getConsCases :: PatM m (VarName, [(Pat, Pattern)])
        getConsCases = do
          headVar <- checkNoCases $ V.headM pattern'args
          rest    <- checkNoCases $ tailM pattern'args
          ps      <- (mapM (fromGroup rest) . groupPats) =<< mapM splitPatCase pattern'pats
          return (headVar, ps)
          where
            splitPatCase PatCase{..} = do
              (h, ts) <- checkNoCases $ headTailM patCase'lhs
              return $ (h, PatCase ts patCase'rhs)

            groupPats =
              L.groupBy ((==) `on` ignoreLoc . fst) . V.toList

            fromGroup args xs = case xs of
              (pat, _): _ -> pure (pat, Pattern args (V.fromList $ fmap snd xs) pattern'other)
              []          -> throwError NoCasesLeft


    -- mixed rule
    fromMixed = combine =<< groupPats pattern'pats
      where
        groupPats ps =
          fmap (joinToPats . groupSamePats) $ mapM getHeadPat ps

        groupSamePats = L.groupBy sameTypePat . V.toList

        sameTypePat = (==) `on` (isConsPat . fst)

        joinToPats = fmap (\xs -> V.fromList $ fmap snd xs)

        combine ps =
          foldM (\other p -> toCase (Pattern pattern'args p other)) pattern'other (reverse ps)

        getHeadPat p@PatCase{..} = fmap (, p) $ checkNoCases $ V.headM patCase'lhs

checkMaybe :: Monad m => PatError -> Maybe a -> PatM m a
checkMaybe err = maybe (throwError err) pure

checkNoCases, checkNoVarFound :: Monad m => Maybe a -> PatM m a

checkNoCases    = checkMaybe NoCasesLeft
checkNoVarFound = checkMaybe NoVarFound

----------------------------------------------------
-- PatCase

isHeadVar :: PatCase -> Bool
isHeadVar PatCase{..} =
  maybe False isVarPat $ V.headM patCase'lhs

isHeadCons :: PatCase -> Bool
isHeadCons = not . isHeadVar

substVarPat :: Monad m => VarName -> PatCase -> PatM m PatCase
substVarPat argVar PatCase{..} = do
  lhs' <- checkNoCases $ tailM patCase'lhs
  localVar <- checkNoVarFound $ getVarPat =<< V.headM patCase'lhs
  let rhs' = subst patCase'rhs localVar (Fix $ Var (getLoc argVar) argVar)
  return $ PatCase lhs' rhs'

----------------------------------------------------
-- utils

headTailM :: Vector a -> Maybe (a, Vector a)
headTailM v = liftA2 (,) (V.headM v) (tailM v)

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
getVarPat = \case
  PVar _ var -> Just var
  _          -> Nothing


data PatNoLoc
  = PVar' VarName
  | PPrim' Prim
  | PCons' ConsName [PatNoLoc]
  | PTuple' [PatNoLoc]
  | PWildCard'
  deriving (Show, Eq, Ord)

ignoreLoc :: Pat -> PatNoLoc
ignoreLoc = \case
  PVar _  v       -> PVar' v
  PPrim _ p       -> PPrim' p
  PCons _ name ps -> PCons' name (fmap ignoreLoc ps)
  PTuple _ ps     -> PTuple' (fmap ignoreLoc ps)
  PWildCard _     -> PWildCard'


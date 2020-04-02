module Hschain.Utxo.Lang.Desugar.PatternCompiler(
    PatError
  , altGroupToExpr
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Data.Fix
import Data.Function (on)
import Data.Maybe
import Data.Vector (Vector)

import Language.HM (getLoc)

import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Exec.Subst
import Hschain.Utxo.Lang.Desugar.FreshVar
import Hschain.Utxo.Lang.Monad

import qualified Data.List   as L
import qualified Data.Vector as V

altGroupToExpr :: MonadLang m => [Alt Lang] -> m Lang
altGroupToExpr = toCaseLam <=< toPatternInput

toPatternInput :: MonadLang m => [Alt Lang] -> m Pattern
toPatternInput alts
  | checkArgs alts = getPattern
  | otherwise      = throwError $ PatternError NoSameArgsNumber
  where
    checkArgs = sameLength . fmap alt'pats

    sameLength = (== 1) . length . L.nub . fmap length

    getPattern = do
      locs <- getFirstLocs alts
      headVars <- mapM getFreshVar locs
      return $ Pattern
        { pattern'args  = V.fromList headVars
        , pattern'pats  = V.fromList $ fmap (\Alt{..} -> PatCase (V.fromList alt'pats) alt'expr) alts
        , pattern'other = failCase
        }

    getFirstLocs = \case
      a:_ -> return $ fmap getLoc $ alt'pats a
      []  -> throwError $ PatternError EmptyArgument

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

toCaseLam :: forall m . MonadLang m => Pattern -> m Lang
toCaseLam p = fmap (\body -> Fix $ LamList noLoc args body) $ toCaseBody p
  where
    args = fmap (\x -> PVar (getLoc x) x) $ V.toList $ pattern'args p


toCaseBody :: forall m . MonadLang m => Pattern -> m Lang
toCaseBody pattern@Pattern{..}
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

    fromVar = (toCaseBody =<<) $ do
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
        toConsCase :: VarName -> [(Pat, Pattern)] -> m Lang
        toConsCase var pats =
          fmap (\xs -> Fix $ CaseOf loc (Fix $ Var loc var) xs) $ mapM toCaseExpr pats
          where
            loc = getLoc var

            toCaseExpr (lhs, rhs) = fmap (CaseExpr lhs) $ toCaseBody rhs

        getConsCases :: m (VarName, [(Pat, Pattern)])
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
              []          -> throwError $ PatternError NoCasesLeft


    -- mixed rule
    fromMixed = combine =<< groupPats pattern'pats
      where
        groupPats ps =
          fmap (joinToPats . groupSamePats) $ mapM getHeadPat ps

        groupSamePats = L.groupBy sameTypePat . V.toList

        sameTypePat = (==) `on` (isNotVarPat . fst)

        joinToPats = fmap (\xs -> V.fromList $ fmap snd xs)

        combine ps =
          foldM (\other p -> toCaseBody (Pattern pattern'args p other)) pattern'other (reverse ps)

        getHeadPat p@PatCase{..} = fmap (, p) $ checkNoCases $ V.headM patCase'lhs

checkMaybe :: MonadLang m => PatError -> Maybe a -> m a
checkMaybe err = maybe (throwError $ PatternError err) pure

checkNoCases, checkNoVarFound :: MonadLang m => Maybe a -> m a

checkNoCases    = checkMaybe NoCasesLeft
checkNoVarFound = checkMaybe NoVarFound

----------------------------------------------------
-- PatCase

isHeadVar :: PatCase -> Bool
isHeadVar PatCase{..} =
  maybe False isVarPat $ V.headM patCase'lhs

isHeadCons :: PatCase -> Bool
isHeadCons = not . isHeadVar

substVarPat :: MonadLang m => VarName -> PatCase -> m PatCase
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
isVarPat = isJust . getVarPat

isNotVarPat :: Pat -> Bool
isNotVarPat = not . isVarPat

isConsPat :: Pat -> Bool
isConsPat = \case
  PCons _ _ _ -> True
  _           -> False

getPrimPat :: Pat -> Maybe (Loc, Prim)
getPrimPat = \case
  PPrim loc p -> Just (loc, p)
  _           -> Nothing

getConsPat :: Pat -> Maybe (Loc, ConsName, [Pat])
getConsPat = \case
  PCons loc name ps -> Just (loc, name, ps)
  _                 -> Nothing

getTuplePat :: Pat -> Maybe (Loc, [Pat])
getTuplePat = \case
  PTuple loc ps -> Just (loc, ps)
  _             -> Nothing

getWildCardPat :: Pat -> Maybe Loc
getWildCardPat = \case
  PWildCard loc -> Just loc
  _             -> Nothing

getVarPat :: Pat -> Maybe VarName
getVarPat = \case
  PVar _ var -> Just var
  _          -> Nothing

data GroupCons a = GroupCons
  { groupCons'prim  :: [(Prim, [a])]
  , groupCons'cons  :: [(ConsName, [([Pat], a)])]
  , groupCons'tuple :: [([Pat], a)]
  , groupCons'wild  :: [a]
  }
  deriving (Show)

groupConsPats :: [(Pat, a)] -> GroupCons a
groupConsPats xs = GroupCons
  { groupCons'prim  = groupPrims xs
  , groupCons'cons  = groupCons xs
  , groupCons'tuple = groupTuples xs
  , groupCons'wild  = groupWilds xs
  }
  where
    groupPrims as =
      regroupPairs $ groupSortOn fst $ catMaybes $ onPair (fmap snd . getPrimPat) as

    groupCons as = regroup $ groupSortOn (fst . fst) $
      catMaybes $ onPair (fmap (\(_, a, b) -> (a, b)) . getConsPat) as
      where
        regroup = mapMaybe $ \x -> case x of
          (a,_):_ -> Just $ (fst a, fmap (\((_, ps), expr) -> (ps, expr)) x)
          []         -> Nothing

    groupTuples as = catMaybes $ onPair (fmap snd . getTuplePat) as

    groupWilds as = fmap snd $ catMaybes $ onPair getWildCardPat as

    onPair f = fmap (\(a, b) -> fmap (, b) $ f a)

regroupPairs :: [[(a, b)]] -> [(a, [b])]
regroupPairs = mapMaybe $ \case
  (a,b):rest -> Just (a, b : fmap snd rest)
  []         -> Nothing

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)


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


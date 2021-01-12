-- | Module defines functions to convert patterns in arguments of functions
-- to case expressions and simplify patterns (make them flat).
module Hschain.Utxo.Lang.Desugar.PatternCompiler(
    PatError
  , altGroupToExpr
  , altGroupToTupleExpr
  , altGroupToTupleModule
  , altToExpr
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Data.Fix
import Data.Function (on)
import Data.Maybe
import Data.Vector (Vector)

import Type.Check.HM (getLoc)

import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Core.Types (Prim(..))
import Hschain.Utxo.Lang.Exec.Subst
import Hschain.Utxo.Lang.Desugar.FreshVar
import Hschain.Utxo.Lang.Desugar.Guard
import Hschain.Utxo.Lang.Monad

import qualified Data.List   as L
import qualified Data.Vector as V

import qualified Type.Check.HM as H

altGroupToTupleModule :: MonadLang m => Module -> m Module
altGroupToTupleModule m@Module{..} = do
  binds <- mapM procBind $ binds'decls $ module'binds
  return $ m { module'binds = module'binds { binds'decls = binds } }
  where
    procBind b@FunBind{..} = do
      expr <- altGroupToTupleExpr bind'alts
      return $ b { bind'alts = [Alt [] (UnguardedRhs expr) Nothing] }
    procBind PatBind{..} = unexpected "Pattern bind in the pattern compiler"

altGroupToTupleExpr :: MonadLang m => [Alt Lang] -> m Lang
altGroupToTupleExpr xs = case xs of
  [a] -> do
      let rhs = addWhere whereExprs $ fromGuardedRhs $ alt'expr a
      return $
        if (null $ alt'pats a)
          then rhs
          else Fix $ LamList (H.getLoc $ alt'expr a) (alt'pats a) rhs
  []  -> noCasesLeft
  as  -> do
    let loc = H.getLoc $ alt'expr $ head as
        ps = alt'pats $ head as
    vs <- mapM (getFreshVar . H.getLoc) ps
    case vs of
      []  -> emptyArgument
      [v] -> fmap (\alts -> Fix $ Lam loc (patToVar v) $ Fix $ CaseOf loc (toVarArg v) alts) $ extractAlts as
      _   -> fmap (\alts -> Fix $ LamList loc (fmap patToVar vs) $ Fix $ CaseOf loc (toTupleArg vs) alts) $ extractAlts as
  where
    patToVar v = PVar (H.getLoc v) v
    toTupleArg vs = Fix $ Tuple (H.getLoc $ head vs) $ fmap toVarArg $ V.fromList vs
    toVarArg v = Fix $ Var (H.getLoc v) v

    whereExprs = mconcat $ mapMaybe alt'where xs

    extractAlts alts
      | sameArgLength  = mapM toCaseExpr alts
      | otherwise      = noSameArgsNumber
      where
        sameArgLength = length (L.nub $ fmap (length . alt'pats) xs) == 1

        toCaseExpr Alt{..} = fmap (\ps -> CaseExpr ps rhs) $ fromPats alt'pats
          where
            rhs = addSingleWhere alt'where $ fromGuardedRhs alt'expr

        fromPats ps = case ps of
          []  -> emptyArgument
          [p] -> return p
          a:_ -> return $ PTuple (H.getLoc a) ps



-- | Converts list of function definitions with pattern matching to single
-- expression with case-expression.
altGroupToExpr :: MonadLang m => [Alt Lang] -> m Lang
altGroupToExpr xs = do
  ePat <- toPatternInput xs
  case ePat of
    Left expr -> return expr
    Right pat -> (fmap removeEmptyCases . toCaseLam whereExprs <=< substWildCards) pat
  where
    whereExprs = mconcat $ mapMaybe alt'where xs

toPatternInput :: MonadLang m => [Alt Lang] -> m (Either Lang Pattern)
toPatternInput alts = case getSimpleBind alts of
  Just alt -> return $ Left $ altToExpr alt
  Nothing  -> fmap Right $
    if checkArgs alts
      then getPattern
      else throwError $ PatError NoSameArgsNumber
  where
    getSimpleBind xs = case xs of
      [x] -> case alt'pats x of
        _ -> Just x
      _   -> Nothing

    checkArgs = sameLength . fmap alt'pats

    sameLength = (== 1) . length . L.nub . fmap length

    getPattern = do
      locs <- getFirstLocs alts
      headVars <- mapM getFreshVar locs
      return $ Pattern
        { pattern'args  = V.fromList headVars
        , pattern'pats  = V.fromList $ fmap (\Alt{..} -> PatCase (V.fromList alt'pats) $ fromGuardedRhs alt'expr) alts
        , pattern'other = failCase
        }

    getFirstLocs = \case
      a:_ -> return $ fmap getLoc $ alt'pats a
      []  -> throwError $ PatError EmptyArgument

    failCase = Fix $ FailCase noLoc

-- | Converts single function definition with pattern-matching
-- to expression with case-expression
altToExpr :: Alt Lang -> Lang
altToExpr Alt{..} = foldr toArg (addSingleWhere alt'where $ fromGuardedRhs alt'expr) alt'pats
  where
    toArg pat body = Fix $ Lam (getLoc pat) pat body


data Pattern = Pattern
  { pattern'args  :: Vector VarName
  , pattern'pats  :: Vector PatCase
  , pattern'other :: Lang
  }

data PatCase = PatCase
  { patCase'lhs :: Vector Pat
  , patCase'rhs :: Lang
  }

toCaseLam :: forall m . MonadLang m => Binds Lang -> Pattern -> m Lang
toCaseLam whereExprs p = case args of
  [] -> fmap (addWhere whereExprs) $ toCaseBody p
  _  -> fmap ((\body -> Fix $ LamList noLoc args body) . addWhere whereExprs) $ toCaseBody p
  where
    args = fmap (\x -> PVar (getLoc x) x) $ V.toList $ pattern'args p

addWhere :: Binds Lang -> Lang -> Lang
addWhere whereExprs = case binds'decls whereExprs of
  [] -> id
  _  -> Fix . Let noLoc whereExprs

addSingleWhere :: Maybe (Binds Lang) -> Lang -> Lang
addSingleWhere mWhere = maybe id (\x -> Fix . Let noLoc x) mWhere


toCaseBody :: forall m . MonadLang m => Pattern -> m Lang
toCaseBody pattern@Pattern{..}
  | isEmpty   = fromEmpty
  | isVar     = fromVar
  | isCons    = fromCons
  | otherwise = fromMixed
  where
    -- empty rule
    isEmpty = V.null pattern'args

    fromEmpty = return $
      case V.headM pattern'pats of
        Just x  -> patCase'rhs x
        Nothing -> pattern'other

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

    fromCons = do
      headVar <- checkNoCases $ V.headM pattern'args
      rest    <- checkNoCases $ tailM pattern'args
      ps      <- mapM splitPatCase pattern'pats
      fromGroupCons rest pattern'other $ groupConsPats headVar (V.toList ps)
      where
        splitPatCase PatCase{..} = do
          (h, ts) <- checkNoCases $ headTailM patCase'lhs
          return $ (h, PatCase ts patCase'rhs)

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
checkMaybe err = maybe (throwError $ PatError err) pure

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
  { groupCons'caseVar :: VarName
  , groupCons'prim    :: [((Loc, Prim), [a])]
  , groupCons'cons    :: [(ConsName, [([Pat], a)])]
  , groupCons'tuple   :: Maybe (Loc, [([Pat], a)])
  , groupCons'wild    :: Maybe (Loc, [a])
  }
  deriving (Show)

fromGroupCons :: MonadLang m => Vector VarName -> Lang -> GroupCons PatCase -> m Lang
fromGroupCons headArgs errorCase GroupCons{..} = do
  wild <- maybe (pure errorCase) (fromWild errorCase) groupCons'wild
  cons <- fmap (toExprWithError wild) $ mapM (fromCons wild) groupCons'cons
  tup  <- maybe (pure cons) (fromTuple cons) groupCons'tuple
  res  <- fmap (toExprWithError tup) $ mapM (fromPrim tup) groupCons'prim
  return res
  where
    toExpr cases = Fix $ CaseOf loc (Fix $ Var loc groupCons'caseVar) cases
      where
        loc = getLoc groupCons'caseVar

    toExprWithError err cases = toExpr $ cases ++ [CaseExpr (PWildCard noLoc) err ]

    fromPrim other ((loc, p), body) = case body of
      [] -> throwError $ PatError NoCasesLeft
      xs ->
        fmap (CaseExpr (PPrim loc p)) $
          toCaseBody $ Pattern headArgs (toPatCases $ fmap ([], ) xs) other

    fromCons other (cons, xs) = case xs of
      []   -> throwError $ PatError NoCasesLeft
      a:_ -> do
        vars <- mapM (getFreshVar . getLoc) $ fst a
        let pvars = fmap toPVar vars
        fmap (CaseExpr (PCons (getLoc cons) cons pvars)) $
          toCaseBody $ Pattern (V.fromList vars <> headArgs) (toPatCases xs) other

    toPVar v = PVar (getLoc v) v

    toPatCases xs = V.fromList $ fmap toPat xs
      where
        toPat (ps, pc) = pc
          { patCase'lhs = V.fromList ps <> patCase'lhs pc }


    fromTuple other (loc, xs) = fmap (toExpr . pure) $ case xs of
      []  -> throwError $ PatError NoCasesLeft
      a:_ -> do
        vars <- mapM (getFreshVar . getLoc) $ fst a
        let pvars = fmap toPVar vars
        fmap (CaseExpr (PTuple loc pvars)) $
          toCaseBody $ Pattern (V.fromList vars <> headArgs) (toPatCases xs) other

    fromWild other (_loc, xs) =
      toCaseBody $ Pattern headArgs (V.fromList xs) other


groupConsPats :: VarName -> [(Pat, a)] -> GroupCons a
groupConsPats headVar xs = GroupCons
  { groupCons'caseVar = headVar
  , groupCons'prim    = groupPrims xs
  , groupCons'cons    = groupCons xs
  , groupCons'tuple   = groupTuples xs
  , groupCons'wild    = groupWilds xs
  }
  where
    groupPrims ys =
      regroup $ groupSortOn fst $ catMaybes $ onPair (getPrimPat) ys
      where
        regroup = mapMaybe $ \case
          a:as -> Just (fst a, fmap snd (a:as))
          _    -> Nothing

    groupCons as = regroup $ groupSortOn (fst . fst) $
      catMaybes $ onPair (fmap (\(_, a, b) -> (a, b)) . getConsPat) as
      where
        regroup = mapMaybe $ \x -> case x of
          (a,_):_ -> Just $ (fst a, fmap (\((_, ps), expr) -> (ps, expr)) x)
          []         -> Nothing

    groupTuples ys = case catMaybes $ onPair (getTuplePat) ys of
      []   -> Nothing
      a:as -> Just (fst $ fst a, fmap (\(b, c) -> (snd b, c)) (a:as))


    groupWilds as = regroup $ catMaybes $ onPair getWildCardPat as
      where
        regroup ys = case ys of
          []  -> Nothing
          x:_ -> Just (fst x, fmap snd ys)



    onPair f = fmap (\(a, b) -> fmap (, b) $ f a)

groupSortOn :: Ord b => (a -> b) -> [a] -> [[a]]
groupSortOn f = L.groupBy ((==) `on` f) . L.sortBy (compare `on` f)

---------------------------------------------------

substWildCards :: MonadLang m => Pattern -> m Pattern
substWildCards p = do
  pats <- mapM substPatCase $ pattern'pats p
  return $ p { pattern'pats = pats }
  where
    substPatCase pc@PatCase{..} = do
      lhs <- mapM substPat patCase'lhs
      return $ pc { patCase'lhs = lhs }

    substPat = \case
      PWildCard loc -> fmap (PVar loc) $ getFreshVar loc
      other         -> return other

removeEmptyCases :: Lang -> Lang
removeEmptyCases x = foldFix go x
  where
    go expr = case expr of
      CaseOf _ (Fix (Var _ _)) [CaseExpr (PWildCard _) next] -> next
      _ -> Fix expr


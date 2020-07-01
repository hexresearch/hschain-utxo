-- | We transform case-patterns so that they can contain
-- only constructors or tuples. We eliminate all catch-all
-- variables patterns, wildcards and primitives
module Hschain.Utxo.Lang.Desugar.Case(
    desugarCase
  , desugarCaseExpr
) where

import Hex.Common.Text

import Data.Either
import Data.Fix
import Data.Foldable
import Data.Maybe
import Data.Sequence (Seq)

import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Exec.Subst
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Desugar

import Safe

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Sequence as Seq
import qualified Data.Vector as V

import qualified Language.HM as H

-- | TODO we need to transform patterns in similiar way as Pattern-compiler
-- prior to further transformations!
desugarCase :: MonadLang m => Module -> m Module
desugarCase m = do
  binds <- mapM (mapM (desugarCaseExpr $ module'userTypes m)) $ module'binds m
  return $ m { module'binds = binds }

desugarCaseExpr :: MonadLang m => UserTypeCtx -> Lang -> m Lang
desugarCaseExpr ctx = {-completeConsCaseExpr ctx <=< -} flatternCaseExpr

-- | Completes missing case-expressions, and removes redundant cases
-- like cases that are shadowed by previous catch-all cases.
completeConsCaseExpr :: MonadLang m => UserTypeCtx -> Lang -> m Lang
completeConsCaseExpr ctx = cataM $ \case
  CaseOf loc e alts -> do
    substForVar e $ (\v -> substAlts ctx loc v alts)
  other             -> pure $ Fix other

-- | In this transformation we remove all nested patterns in case expression
-- We reuse pattern compiler for functions to process case-expressions in similiar way.
{-
flatternCaseExpr :: MonadLang m => Lang -> m Lang
flatternCaseExpr = cataM $ \case
  CaseOf loc e caseAlts -> fmap (toResultExpr loc e) $ altGroupToExpr $ fmap toAlt caseAlts
  other                 -> pure $ Fix other
  where
    toAlt CaseExpr{..} =
      Alt
        { alt'pats = [caseExpr'lhs]
        , alt'expr = UnguardedRhs caseExpr'rhs
        ,alt'where = Nothing
        }

    toResultExpr loc e caseExpr = case caseExpr of
      Fix (LamList _ [_] (Fix (CaseOf _ _ alts))) -> Fix $ CaseOf loc e alts
      _                                           -> Fix $ Apply loc caseExpr e
-}

flatternCaseExpr :: MonadLang m => Lang -> m Lang
flatternCaseExpr (Fix x) = case x of
  CaseOf loc e alts -> do
    ev <- getCaseVar e
    let v = either id id ev
        eloc = H.getLoc e
    alts' <- fmap toList $ flattenAltGroup v Seq.empty S.empty alts
    return $ if null alts'
      then Fix $ FailCase loc
      else case ev of
        Right _ -> Fix $ Let loc [simpleBind v e] $ Fix $ CaseOf loc (Fix $ Var eloc v) alts'
        Left  _ -> Fix $ CaseOf loc (Fix $ Var eloc v) alts'
  other             -> fmap Fix $ mapM flatternCaseExpr other
  where
    getCaseVar (Fix e) = case e of
      Var _ v -> return $ Left v
      _       -> fmap Right $ getFreshVar (H.getLoc e)

    flattenAltGroup topExpr res definedConses alts = case alts of
      []   -> return res
      a:as -> flattenAlt topExpr res definedConses as a

    flattenAlt topExpr res definedConses rest CaseExpr{..} = do
      rhs <- flatternCaseExpr caseExpr'rhs
      case lhs of
        PVar _ _    -> return $ res Seq.|> (CaseExpr lhs rhs)
        PPrim _ _   -> flattenAltGroup topExpr (res Seq.|> CaseExpr lhs rhs) definedConses rest
        PWildCard loc -> do
          v <- getFreshVar loc
          return $ res Seq.|> CaseExpr (PVar loc v) rhs
        PCons loc name ps ->
          if S.member name definedConses
            then flattenAltGroup topExpr res definedConses rest
            else flattenListPat topExpr res (definedConses <> S.singleton name) rest rhs (PCons loc name) ps
        PTuple loc ps     ->
          if hasTuple res
            then return res
            else flattenListPat topExpr res definedConses rest rhs (PTuple loc) ps
      where
        lhs = caseExpr'lhs

    hasTuple :: Seq (CaseExpr Lang) -> Bool
    hasTuple = any (isTuple . caseExpr'lhs)

    isTuple :: Pat -> Bool
    isTuple = \case
      PTuple _ _ -> True
      _          -> False

    flattenListPat topExpr res definedConses rest rhs pcons ps = do
        (ps', complexPats) <- flattenPats ps
        if null complexPats
          then flattenAltGroup topExpr (res Seq.|> CaseExpr (pcons ps') rhs) definedConses rest
          else do
            contAlts <- flattenAltGroup topExpr Seq.empty S.empty rest
            let topLoc = H.getLoc topExpr
                cont = if null contAlts
                        then Fix $ FailCase topLoc
                        else Fix $ CaseOf topLoc (Fix $ Var topLoc topExpr) $ toList contAlts
                (primPats, plainPats) = splitNestedPats complexPats
            rhs' <- fromNestedPat cont primPats plainPats rhs
            flattenAltGroup topExpr (res Seq.|> CaseExpr (pcons ps') rhs') definedConses rest

    splitNestedPats xs = partitionEithers $ fmap splitPat xs
      where
        splitPat (v, pat) = case pat of
          NestedPat p -> Right (v, p)
          NestedPrim loc p -> Left (v, loc, p)

    fromNestedPat cont primPats pats rhs =
      case headMay primPats of
        Nothing -> fromComplexPat cont pats rhs
        Just headPrim -> do
          rhs' <- fromComplexPat cont pats rhs
          let exprLoc  = (\(_, src, _) -> src) headPrim
              cond = L.foldl1 and' $ fmap (\(v, loc, p) -> eq loc (Fix $ Var loc v) (Fix $ PrimE loc p)) primPats
          return $ Fix $ If exprLoc cond rhs' cont
      where
        eq loc a b = Fix $ BinOpE loc Equals a b
        and' a b = Fix $ BinOpE (H.getLoc a) And a b

    fromComplexPat cont pats rhs = do
      case pats of
        [] -> return rhs
        (v, p) : ps -> do
            let loc = H.getLoc v
            otherV <- getFreshVar loc
            rhs' <- fromComplexPat cont ps rhs
            alts <- flattenAltGroup v Seq.empty S.empty
              [ CaseExpr
                  { caseExpr'lhs = p
                  , caseExpr'rhs = rhs'
                  }
              , CaseExpr
                  { caseExpr'lhs = PVar loc otherV
                  , caseExpr'rhs = cont
                  }
              ]
            return $ Fix $ CaseOf loc (Fix $ Var loc v) $ toList alts

    flattenPats ps =
      fmap (\(args, complexPats) -> (reverse args, reverse complexPats)) $ foldM fromPat ([], []) ps
      where
        fromPat (args, complexPats) = \case
          PVar loc v -> return (PVar loc v : args, complexPats)
          PPrim loc p -> do
            v <- getFreshVar loc
            return (PVar loc v : args, (v, NestedPrim loc p) : complexPats)
          PWildCard loc -> do
            v <- getFreshVar loc
            return (PVar loc v : args, complexPats)
          other -> do
            let loc = H.getLoc other
            v <- getFreshVar loc
            return (PVar loc v : args, (v, NestedPat other) : complexPats)

data NestedPat
  = NestedPrim Loc Prim
  | NestedPat  Pat
  deriving (Show, Eq)


substForVar :: MonadLang m => Lang -> (Lang -> m Lang) -> m Lang
substForVar e cont = case e of
  Fix (Var _ _) -> cont e
  expr          -> do
    let loc = H.getLoc expr
    v <- getFreshVar loc
    let toLet body = Fix $ Let loc [simpleBind v expr] body
    fmap toLet $ cont (Fix $ Var loc v)

data LegalConsPat = ConsPat | TuplePat
  deriving (Show, Eq)

substAlts :: forall m . MonadLang m => UserTypeCtx -> Loc -> Lang -> [CaseExpr Lang] -> m Lang
substAlts ctx loc expr alts
  | allCons alts = return $ simpleCase loc expr alts
  | otherwise    = case alts of
    []   -> noCasesLeft
    a:as -> case caseExpr'lhs a of
      PPrim ploc p   -> fromPrim ploc p (caseExpr'rhs a) as
      PVar  _ v      -> fromVar  v (caseExpr'rhs a)
      PWildCard ploc -> fromWildCard ploc (caseExpr'rhs a)
      PCons _ _ _    -> collectCons ConsPat  [a] as
      PTuple _ _     -> collectCons TuplePat [a] as
  where
    allCons = all (isConsOrTuple . caseExpr'lhs)

    isConsOrTuple = \case
      PCons _ _ _ -> True
      PTuple _ _  -> True
      _           -> False

    fromPrim ploc p rhs rest = case rest of
      []   -> return $ ifPrim $ Fix $ FailCase ploc
      a:as -> case caseExpr'lhs a of
                PPrim ploc2 p2 -> fmap ifPrim $ fromPrim ploc2 p2 (caseExpr'rhs a) as
                PVar _ v       -> return $ ifPrim $ subst rhs v expr
                PWildCard _    -> return $ ifPrim expr
                other          -> wrongPatPrimMixture $ H.getLoc other
      where
        ifPrim e = Fix $ If ploc (Fix $ BinOpE ploc Equals (Fix $ PrimE ploc p) expr) rhs e



    fromVar v rhs = return $ subst rhs v expr

    fromWildCard ploc rhs = do
      v <- getFreshVar ploc
      return $ Fix $ CaseOf loc expr [CaseExpr (PVar ploc v) rhs]

    collectCons legalConsPat res rest = case rest of
      []   -> noCasesLeft
      a:as -> case caseExpr'lhs a of
        PCons _ _ _     | isCons  -> collectCons ConsPat (a:res) as
        PCons ploc _ _            -> wrongPatConsMixture ploc
        PTuple _ _      | isTuple -> collectCons TuplePat (a:res) as
        PTuple ploc _             -> wrongPatConsMixture ploc
        PPrim ploc _              -> wrongPatPrimMixture ploc
        PVar ploc v               -> completeVarCases ctx ploc v (caseExpr'rhs a) (reverse res)
        PWildCard ploc            -> completeWildCardCases ctx ploc (caseExpr'rhs a) (reverse res)
        where
          isCons  = legalConsPat == ConsPat
          isTuple = legalConsPat == TuplePat

    completeVarCases typeCtx ploc v rhs conses = do
      rest <- getRemainConstructorSet typeCtx ploc $ fmap caseExpr'lhs conses
      let caseAlts = conses ++ fmap (toVarCaseAlt v rhs) rest
      return $ Fix $ CaseOf loc expr caseAlts

    completeWildCardCases typeCtx ploc rhs conses = do
      rest <- fmap (fmap fst) $ getRemainConstructorSet typeCtx ploc $ fmap caseExpr'lhs conses
      let caseAlts = conses ++ fmap (toWildCardCaseAlt rhs) rest
      return $ Fix $ CaseOf loc expr caseAlts

    getRemainConstructorSet :: UserTypeCtx -> Loc -> [Pat] -> m [(Pat, Lang)]
    getRemainConstructorSet typeCtx ploc pats =
      case pats of
        []   -> noCasesLeft
        p:_ -> case p of
          PTuple _ _  -> return []
          PCons _ _ _ -> fromUserType
          other       -> failedToEliminate $ mappend "Non cons-cases in case-compiler: " (showt other)
      where
        consNames = mapMaybe getConsName pats
        consNameSet = S.fromList $ fmap consName'name consNames

        getConsInfo name =
          maybe (unboundVariable (consToVarName name)) pure $ M.lookup name (userTypeCtx'constrs typeCtx)

        getConsName = \case
          PCons _ name _ -> Just name
          _              -> Nothing

        fromUserType :: m [(Pat, Lang)]
        fromUserType = do
          infos <- mapM getConsInfo consNames
          if (sameType infos)
            then do
              conses <- getConsList infos
              mapM toResult $ filter (not . isUsedCons) conses
            else wrongPatConsMixture loc

        sameType = isSingleElem . fmap (userType'name . consInfo'def)

        getConsList infos =
          case infos of
            []  -> noCasesLeft
            a:_ -> return $ M.toList $ userType'cases $ consInfo'def a

        isUsedCons (name, _) = S.member (consName'name name) consNameSet

        toResult :: (ConsName, ConsDef) -> m (Pat, Lang)
        toResult (name, def) = do
          vs <- mapM (getFreshVar . const noLoc) [1 .. arity]
          return $ (PCons ploc name $ fmap (PVar ploc) vs, Fix $ Cons ploc name $ V.fromList $ fmap (Fix . Var ploc) vs)
          where
            arity = getConsDefArity def

        isSingleElem xs = case xs of
          []   -> False
          a:as -> all (a ==) as

    toVarCaseAlt v rhs (pat, patExpr) =
      CaseExpr
        { caseExpr'lhs = pat
        , caseExpr'rhs = subst rhs v patExpr
        }

    toWildCardCaseAlt rhs pat =
      CaseExpr
        { caseExpr'lhs = pat
        , caseExpr'rhs = rhs
        }


simpleCase :: Loc -> Lang -> [CaseExpr Lang] -> Lang
simpleCase loc expr alts = Fix $ CaseOf loc expr alts


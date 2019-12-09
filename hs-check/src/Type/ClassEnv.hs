module Type.ClassEnv(
    ClassEnv(..)
  , Inst
  , Class(..)
  , initialEnv
  , reduce
  , defaultSubst
  , defaultedPreds
  , entail
) where

import Control.Applicative
import Control.Monad
import Control.Monad.Except

import Data.Either
import Data.Maybe

import Type.Loc
import Type.Type
import Type.Subst

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S

data Class = Class
  { class'supers      :: [Id]
  , class'instances   :: [Inst]
  }

type Inst = Qual Pred

data ClassEnv = ClassEnv
  { classEnv'classes   :: Id -> Either TypeError Class
  , classEnv'defaults  :: [Type]
  }

super :: ClassEnv -> Id -> [Id]
super ce i = either (const []) class'supers $ classEnv'classes ce i

insts :: ClassEnv -> Id -> [Inst]
insts ce i = either (const []) class'instances $ classEnv'classes ce i

modify :: ClassEnv -> Id -> Class -> ClassEnv
modify ce i c = ce { classEnv'classes = \j ->
  if (i == j)
    then Right c
    else classEnv'classes ce j }

initialEnv :: ClassEnv
initialEnv = ClassEnv
  { classEnv'classes  = \idx ->
      if idx == "Num"
        then Right numClass
        else Left $ singleTypeError (getLoc idx) "Class not defined"
  , classEnv'defaults = [integerT, doubleT]
  }

type EnvTransformer = ClassEnv -> Either TypeError ClassEnv

(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) = (>=>)

defined :: Either TypeError a -> Bool
defined = isRight

addClass :: Id -> [Id] -> EnvTransformer
addClass idx idxs ce
  | defined (classEnv'classes ce idx)               = Left $ singleTypeError (getLoc idx) "class already defined"
  | any (not . defined . classEnv'classes ce) idxs  = Left $ singleTypeError (getLoc idx) "superclass not defined"
  | otherwise                                       = return $ (modify ce idx (Class idxs []))

addPreludeClasses :: EnvTransformer
addPreludeClasses = addCoreClasses <:> addNumClasses

addCoreClasses :: EnvTransformer
addCoreClasses = classes ["Eq", "Show", "Read", "Enum", "Bounded", "Functor", "Monad"]
  <:> addClass "Ord" ["Eq"]
  where
    classes xs = foldl1 (<:>) (fmap (\x -> addClass x []) xs)

addNumClasses :: EnvTransformer
addNumClasses =
      addClass "Num" ["Eq", "Show"]
  <:> addClass "Real" ["Num", "Ord"]
  <:> addClass "Fractional" ["Num"]

addInst :: [Pred] -> Pred -> EnvTransformer
addInst ps p@(IsIn loc idx _) ce
  | not $ defined $ classEnv'classes ce idx = Left $ singleTypeError loc "no class for instance"
  | any (overlap p) qs                      = Left $ singleTypeError loc "overlapping instance"
  | otherwise                               = return $ modify ce idx c
  where
    its = insts ce idx
    qs  = fmap (\(Qual _ _ t) -> t) its
    c   = Class (super ce idx) (Qual loc ps p : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined $ mostGeneralUnifierPred p q

exampleInsts :: EnvTransformer
exampleInsts = addPreludeClasses
  <:> addOrds
  <:> addPair
  where
    isOrd = IsIn noLoc "Ord"

    addOrds = foldl1 (<:>) (fmap (\x -> addInst [] (isOrd x)) [unitT, charT, intT, doubleT])

    addPair = addInst [isOrd a, isOrd b] (isOrd $ pair a b)
      where
        a = var "a"
        b = var "b"


bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn loc idx ty) =
  p : concat [ bySuper ce (IsIn loc idx' ty) | idx' <- super ce idx ]

byInst :: ClassEnv -> Pred -> Except TypeError [Pred]
byInst ce p@(IsIn _ idx t) = foldr (<|>) empty $ fmap tryInst $ insts ce idx
  where
    tryInst (Qual _ ps h) = do
      u <- matchPred h p
      return (map (apply u) ps)

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = cond1 || cond2
  where
    cond1 = any (p `elem`) (map (bySuper ce) ps)
    cond2 = either (const False) (all (entail ce ps)) $ runExcept $ byInst ce p


isHnf :: Pred -> Bool
isHnf (IsIn _ _ t) = hnf t
  where
    hnf = \case
      TVar _ v   -> True
      TCon _ tc  -> False
      TFun _ _ _ -> False
      TAp  _ t _ -> hnf t
      _          -> False

toHnfs :: ClassEnv -> [Pred] -> Except TypeError [Pred]
toHnfs ce ps = fmap concat $ mapM (toHnf ce) ps

toHnf :: ClassEnv -> Pred -> Except TypeError [Pred]
toHnf ce p
  | isHnf p   = return [p]
  | otherwise = catchError (toHnfs ce =<< byInst ce p) (const err)
  where
    err = throwError $ singleTypeError (getLoc p) "context reduction"

simplify :: ClassEnv -> [Pred] -> [Pred]
simplify ce = loop []
  where
    loop rs = \case
      []                             -> rs
      p:ps  | entail ce (rs ++ ps) p -> loop rs ps
            | otherwise              -> loop (p : rs) ps


reduce :: ClassEnv -> [Pred] -> Except TypeError [Pred]
reduce ce ps = fmap (simplify ce) $ toHnfs ce ps
  where
    simplify :: ClassEnv -> [Pred] -> [Pred]
    simplify ce = loop []
      where
        loop rs = \case
          []                               -> rs
          p:ps  | scEntail ce (rs ++ ps) p -> loop rs ps
                | otherwise                -> loop (p : rs) ps

    scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

ambiguities :: [Tyvar] -> [Pred] -> [Ambiguity]
ambiguities vs ps = [ Ambiguity v (filter (elem v . getVars) ps) | v <- (S.toList $ getVars ps) L.\\ vs]

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (Ambiguity v qs) =
  [ t' | let is = [ i | IsIn _ i t <- qs ]
             ts = [ t | IsIn _ i t <- qs ],
         all ((TVar noLoc v) == ) ts,
         any (`elem` numClasses) is,
         all (`elem` stdClasses) is,
         t' <- classEnv'defaults ce,
         all (entail ce []) [IsIn (getLoc i) i t' | i <- is]]

withDefaults :: MonadError TypeError m => ([Ambiguity] -> [Type] -> a) -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps = case L.find (null . snd) vpTss of
  Just (vp, _) -> throwError $ singleTypeError (getLoc vp) "can not resolve ambiguity"
  Nothing      -> return (f vps (map head tss))
  where
    vps = ambiguities vs ps
    tss = fmap snd vpTss
    vpTss = map (\vp -> (vp, candidates ce vp)) vps


defaultedPreds :: MonadError TypeError m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults (\vps ts -> concat $ map ambiguity'preds vps)

defaultSubst :: MonadError TypeError m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst = withDefaults (\vps ts -> Subst $ M.fromList $ zip (map ambiguity'tyvar vps) ts)

numClass :: Class
numClass = Class
  { class'supers      = ["Eq", "Show"]
  , class'instances   = [Qual noLoc [] (IsIn noLoc "Num" intT), Qual noLoc [] (IsIn noLoc "Num" doubleT)]
  }

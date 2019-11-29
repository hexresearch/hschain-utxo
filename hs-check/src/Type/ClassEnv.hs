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
        else Left $ TypeError "Class not defined"
  , classEnv'defaults = [integerT, doubleT]
  }

type EnvTransformer = ClassEnv -> Either TypeError ClassEnv

(<:>) :: EnvTransformer -> EnvTransformer -> EnvTransformer
(<:>) = (>=>)

defined :: Either TypeError a -> Bool
defined = isRight

addClass :: Id -> [Id] -> EnvTransformer
addClass idx idxs ce
  | defined (classEnv'classes ce idx)               = Left $ TypeError "class already defined"
  | any (not . defined . classEnv'classes ce) idxs  = Left $ TypeError "superclass not defined"
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
addInst ps p@(IsIn idx _) ce
  | not $ defined $ classEnv'classes ce idx = fail "no class for instance"
  | any (overlap p) qs                      = fail "overlapping instance"
  | otherwise                               = return $ modify ce idx c
  where
    its = insts ce idx
    qs  = fmap (\(Qual _ t) -> t) its
    c   = Class (super ce idx) (Qual ps p : its)

overlap :: Pred -> Pred -> Bool
overlap p q = defined $ mostGeneralUnifierPred p q

exampleInsts :: EnvTransformer
exampleInsts = addPreludeClasses
  <:> addOrds
  <:> addPair
  where
    isOrd = IsIn "Ord"

    addOrds = foldl1 (<:>) (fmap (\x -> addInst [] (isOrd x)) [unitT, charT, intT, doubleT])

    addPair = addInst [isOrd a, isOrd b] (isOrd $ pair a b)
      where
        a = var "a"
        b = var "b"


bySuper :: ClassEnv -> Pred -> [Pred]
bySuper ce p@(IsIn idx ty) =
  p : concat [ bySuper ce (IsIn idx' ty) | idx' <- super ce idx ]

byInst :: ClassEnv -> Pred -> Except TypeError [Pred]
byInst ce p@(IsIn idx t) = foldr (<|>) empty $ fmap tryInst $ insts ce idx
  where
    tryInst (Qual ps h) = do
      u <- matchPred h p
      return (map (apply u) ps)

entail :: ClassEnv -> [Pred] -> Pred -> Bool
entail ce ps p = cond1 || cond2
  where
    cond1 = any (p `elem`) (map (bySuper ce) ps)
    cond2 = either (const False) (all (entail ce ps)) $ runExcept $ byInst ce p


isHnf :: Pred -> Bool
isHnf (IsIn _ t) = hnf t
  where
    hnf = \case
      TVar v   -> True
      TCon tc  -> False
      TAp  t _ -> hnf t

toHnfs :: ClassEnv -> [Pred] -> Except TypeError [Pred]
toHnfs ce ps = fmap concat $ mapM (toHnf ce) ps

toHnf :: ClassEnv -> Pred -> Except TypeError [Pred]
toHnf ce p
  | isHnf p   = return [p]
  | otherwise = catchError (toHnfs ce =<< byInst ce p) (const err)
  where
    err = throwError $ TypeError "context reduction"

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
ambiguities vs ps = [(v , filter (elem v . getVars) ps) | v <- (S.toList $ getVars ps) L.\\ vs]

candidates :: ClassEnv -> Ambiguity -> [Type]
candidates ce (v, qs) =
  [ t' | let is = [ i | IsIn i t <- qs ]
             ts = [ t | IsIn i t <- qs ],
         all ((TVar v) == ) ts,
         any (`elem` numClasses) is,
         all (`elem` stdClasses) is,
         t' <- classEnv'defaults ce,
         all (entail ce []) [IsIn i t' | i <- is]]

withDefaults :: MonadError TypeError m => ([Ambiguity] -> [Type] -> a) -> ClassEnv -> [Tyvar] -> [Pred] -> m a
withDefaults f ce vs ps
  | any null tss = throwError $ TypeError "can not resolve ambiguity"
  | otherwise    = return (f vps (map head tss))
  where
    vps = ambiguities vs ps
    tss = map (candidates ce) vps

defaultedPreds :: MonadError TypeError m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
defaultedPreds = withDefaults (\vps ts -> concat $ map snd vps)

defaultSubst :: MonadError TypeError m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
defaultSubst = withDefaults (\vps ts -> Subst $ M.fromList $ zip (map fst vps) ts)

numClass :: Class
numClass = Class
  { class'supers      = ["Eq", "Show"]
  , class'instances   = [Qual [] (IsIn "Num" intT), Qual [] (IsIn "Num" doubleT)]
  }

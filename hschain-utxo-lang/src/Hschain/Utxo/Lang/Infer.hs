module Hschain.Utxo.Lang.Infer where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Fix hiding ((~>))
import Data.Vector (Vector)

import Type.ClassEnv
import Type.Infer
import Type.Subst
import Type.Type

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

infixr ~>

(~>) = fn

inferPrim :: Prim -> Infer (Qual Type)
inferPrim = \case
  PrimInt _     -> numT
  PrimMoney  _  -> numT
  PrimDouble _  -> numT
  PrimString _  -> return $ Qual [] textT
  PrimBool _    -> return $ Qual [] boolT
  where
    numT = do
      v <- newTVar Star
      return $ Qual [IsIn "Num" v] v

inferExpr :: [Assump] -> Lang -> Infer (Qual Type)
inferExpr as (Fix x) = case x of
  Var varName        -> fromVar as varName
  Apply a b          -> fromApply as a b
  Lam varName a      -> fromLam as varName a
  LamList vars a     -> fromLamList as vars a
  Let varName a      -> fromLet as varName a
  LetArg varName args a b -> fromLetArg as varName args a b
  LetRec varName a b -> fromLetRec as varName a b
  Ascr a ty          -> fromAscr as a ty
  -- primitives
  PrimE p            -> fromPrim p
  -- logic
  If a b c           -> fromIf as a b c
  Pk a               -> fromPk as a
  -- tuples
  Tuple t            -> fromTuple as t
  -- operations
  UnOpE op a         -> fromUnOp as op a
  BinOpE op a b      -> fromBinOp as op a b
  -- environment
  GetEnv envId       -> fromGetEnv as envId
  -- vectors
  VecE vec           -> fromVec as vec
  -- text
  TextE txt          -> fromText as txt
  -- boxes
  BoxE box           -> fromBox as box
  -- undef value
  Undef              -> fromUndef
  -- debug
  Trace a b          -> fromTrace as a b

fromVar :: [Assump] -> VarName -> Infer (Qual Type)
fromVar as v = do
  sc <- findAssump v as
  freshInst sc

fromApply :: [Assump] -> Lang -> Lang -> Infer (Qual Type)
fromApply as f a = do
  Qual pf tf <- inferExpr as f
  Qual pa ta <- inferExpr as a
  t <- newTVar Star
  unify (ta `fn` t) tf
  return $ Qual (pf ++ pa) t

fromLam :: [Assump] -> VarName -> Lang -> Infer (Qual Type)
fromLam as v b = inferExpr as $ toLet v b
  where
    toLet :: VarName -> Lang -> Lang
    toLet arg body = Fix $ LetArg "f" ["x"] body (Fix $ Var "f")

fromLamList :: [Assump] -> [VarName] -> Lang -> Infer (Qual Type)
fromLamList as args body = inferExpr as $ unfoldLamList args body

fromLet as bg e = do
  (ps, as') <- inferBindGroup as bg
  Qual qs t <- inferExpr (as' ++ as) e
  return $ Qual (ps ++ qs) t

fromLetArg :: [Assump] -> VarName -> [VarName] -> Lang -> Lang -> Infer (Qual Type)
fromLetArg as v vs body expr = inferExpr as $ unfoldLetArg v vs body expr

fromLetRec = undefined

fromAscr as a t = do
  Qual aPs aT <- inferExpr as a
  unify aT t
  return $ Qual aPs t

fromPrim :: Prim -> Infer (Qual Type)
fromPrim = inferPrim

fromIf :: [Assump] -> Lang -> Lang -> Lang -> Infer (Qual Type)
fromIf as cond a b = do
  Qual condPs condT <- inferExpr as cond
  Qual aPs aT <- inferExpr as a
  Qual bPs bT <- inferExpr as b
  unify condT boolT
  unify aT bT
  return $ Qual (condPs ++ aPs ++ bPs) aT

fromPk :: [Assump] -> Lang -> Infer (Qual Type)
fromPk as a = do
  Qual aPs aT <- inferExpr as a
  unify aT textT
  return $ Qual aPs textT

fromTuple :: [Assump] -> Vector Lang -> Infer (Qual Type)
fromTuple as vs = do
      ts <- mapM (const $ newTVar Star) vs
      qs <- mapM (inferExpr as) vs
      let ts' = fmap (\(Qual _ t) -> t) qs
          ps  = fmap (\(Qual p _) -> p) qs
      zipWithM unify (V.toList ts) (V.toList ts')
      return $ Qual (concat $ V.toList ps) (tupleT $ V.toList ts')

fromVec :: [Assump] -> VecExpr Lang -> Infer (Qual Type)
fromVec as = \case
  NewVec vs -> do
    t <- newTVar Star
    ps <- fmap concat $ mapM ((\(Qual ps ty) -> unify t ty >> return ps) <=< inferExpr as) vs
    return $ Qual ps $ vectorT t
  VecAppend a b -> do
    Qual aPs aT <- inferExpr as a
    Qual bPs bT <- inferExpr as b
    t <- newTVar Star
    unify (vectorT t) aT
    unify (vectorT t) bT
    return $ Qual (aPs ++ bPs) (vectorT t)
  VecAt a b -> do
    t <- newTVar Star
    Qual aPs aT <- inferExpr as a
    Qual bPs bT <- inferExpr as b
    unify (vectorT t) aT
    unify bT intT
    return $ Qual aPs t
  VecLength -> do
    t <- newTVar Star
    return $ Qual [] (t ~> intT)
  VecMap -> do
    a <- newTVar Star
    b <- newTVar Star
    return $ Qual [] ( (a ~> b) ~> vectorT a ~> vectorT b)
  VecFold    -> do
    a <- newTVar Star
    b <- newTVar Star
    return $ Qual [] ( (a ~> b ~> a) ~> a ~> vectorT b ~> a)

fromUnOp :: [Assump] -> UnOp -> Lang -> Infer (Qual Type)
fromUnOp as = \case
  Not       -> fromNot
  Neg       -> fromNeg
  TupleAt n -> fromTupleAt n
  where
    fromNot a = do
      Qual _ aT <- inferExpr as a
      unify aT boolT
      return $ Qual [] boolT

    fromNeg a = do
      t <- newTVar Star
      Qual aPs aT <- inferExpr as a
      unify t aT
      return $ Qual ([IsIn "Num" t] ++ aPs) t

    fromTupleAt n vs = do
      Qual ps ts <- inferExpr as vs
      extractTuple n ps ts
      where
        extractTuple n ps ts = case (V.fromList $ tupleArgs ts) V.!? n of
          Just t   -> return $ Qual ps t
          Nothing  -> throwError $ TypeError "Wrong size tuple"

        tupleArgs = reverse . go []
          where
            go res ts = case ts of
              TAp a b -> go (b : res) a
              _       -> res

fromBinOp :: [Assump] -> BinOp -> Lang -> Lang -> Infer (Qual Type)
fromBinOp as = \case
  And                 -> boolOp
  Or                  -> boolOp
  Plus                -> numOp
  Minus               -> numOp
  Times               -> numOp
  Div                 -> numOp
  Equals              -> eqOp
  NotEquals           -> eqOp
  LessThan            -> ordOp
  GreaterThan         -> ordOp
  LessThanEquals      -> ordOp
  GreaterThanEquals   -> ordOp
  ComposeFun          -> funOp
  where
    boolOp a b = do
      Qual aPs aT <- inferExpr as a
      Qual bPs bT <- inferExpr as b
      unify aT boolT
      unify bT boolT
      return $ Qual [] boolT

    numOp a b = do
      t <- newTVar Star
      Qual aPs aT <- inferExpr as a
      Qual bPs bT <- inferExpr as b
      unify t aT
      unify t bT
      return $ Qual ((IsIn "Num" t) : (aPs ++ bPs)) t

    eqOp a b = do
      t <- newTVar Star
      Qual aPs aT <- inferExpr as a
      Qual bPs bT <- inferExpr as b
      unify t aT
      unify t bT
      return $ Qual [] boolT

    ordOp a b = do
      t <- newTVar Star
      Qual aPs aT <- inferExpr as a
      Qual bPs bT <- inferExpr as b
      unify t aT
      unify t bT
      return $ Qual [] boolT

    funOp f g = do
      Qual fPs fT <- inferExpr as f
      Qual gPs gT <- inferExpr as g
      aT <- newTVar Star
      bT <- newTVar Star
      cT <- newTVar Star
      unify fT (bT ~> cT)
      unify gT (aT ~> bT)
      return $ Qual (fPs ++ gPs) ((bT ~> cT) ~> (aT ~> bT) ~> aT ~> cT)

fromGetEnv :: [Assump] -> EnvId Lang -> Infer (Qual Type)
fromGetEnv as = \case
  Height      -> return $ Qual [] intT
  Input a     -> fromBoxGet a
  Output a    -> fromBoxGet a
  Self        -> return $ Qual [] boxT
  Inputs      -> vecBox
  Outputs     -> vecBox
  GetVar a    -> fromGetVar a
  where
    vecBox = return $ Qual [] $ vectorT boxT

    fromBoxGet a = do
      Qual _ aT <- inferExpr as a
      unify aT intT
      return $ Qual [] boxT

    fromGetVar arg = do
      t <- newTVar Star
      Qual _ argT <- inferExpr as arg
      unify argT textT
      return $ Qual [] t

fromText :: [Assump] -> TextExpr Lang -> Infer (Qual Type)
fromText as = \case
  TextAppend a b -> do
    Qual _ aT <- inferExpr as a
    Qual _ bT <- inferExpr as b
    unify aT textT
    unify bT textT
    return $ Qual [] textT
  ConvertToText  -> do
    t <- newTVar Star
    return $ Qual [] $ t ~> textT
  TextLength     -> return $ Qual [] $ textT ~> intT
  TextHash _     -> return $ Qual [] $ textT ~> textT

fromBox :: [Assump] -> BoxExpr Lang -> Infer (Qual Type)
fromBox as = \case
  PrimBox _ -> return $ Qual [] boxT
  BoxAt box field -> fromBoxField box field
  where
    fromBoxField box field = do
      Qual boxPs bT <- inferExpr as box
      unify bT boxT
      case field of
        BoxFieldId      -> return $ Qual [] textT
        BoxFieldValue   -> return $ Qual [] moneyT
        BoxFieldScript  -> return $ Qual [] scriptT
        BoxFieldArg txt -> do
          t <- newTVar Star
          Qual txtPs txtT <- inferExpr as txt
          unify txtT textT
          return $ Qual [] t


fromUndef :: Infer (Qual Type)
fromUndef = fmap (Qual []) $ newTVar Star

fromTrace :: [Assump] -> Lang -> Lang -> Infer (Qual Type)
fromTrace as msg arg = do
  Qual msgPs msgT <- inferExpr as msg
  Qual argPs argT <- inferExpr as arg
  unify msgT textT
  return $ Qual (msgPs ++ argPs) argT


inferBindGroup :: [Assump] -> BindGroup Lang -> Infer ([Pred], [Assump])
inferBindGroup as BindGroup{..} = do
  ce <- getClassEnv
  (ps, as'') <- inferSeq inferImpl (as' ++ as) bindGroup'impl
  qss <- mapM (inferExpl (as'' ++ as' ++ as)) bindGroup'expl
  return $ (ps ++ concat qss, as'' ++ as')
  where
    as' = fmap toAssump bindGroup'expl

    toAssump Expl{..} = expl'name :>: expl'type


inferSeq :: ([Assump] -> e -> Infer ([Pred], [Assump])) ->
            ([Assump] -> [e] -> Infer ([Pred], [Assump]))
inferSeq infer as = \case
  []     -> return ([], [])
  bs:bss -> do
    ce <- getClassEnv
    (ps, as')  <- infer as bs
    (qs, as'') <- inferSeq infer (as' ++ as) bss
    return (ps ++ qs, as'' ++ as')

inferProgram :: ClassEnv -> [Assump] -> Program -> Either TypeError [Assump]
inferProgram ce as bgs = (\inf -> runInfer inf ce) $ do
  (ps, as') <- inferSeq inferBindGroup as bgs
  s <- getSubst
  rs <- Infer $ lift $ reduce ce (apply s ps)
  s' <- defaultSubst ce [] rs
  return $ apply (s' `compose` s) as'

inferAlt :: [Assump] -> Alt Lang -> Infer (Qual Type)
inferAlt as (Alt pats e) = do
  (ps, as', ts) <- inferPats pats
  Qual qs t <- inferExpr (as' ++ as) e
  return $ Qual (ps ++ qs) (foldr fn t ts)

inferAlts :: [Assump] -> [Alt Lang] -> Type -> Infer [Pred]
inferAlts as alts t = do
  psts <- mapM (inferAlt as) alts
  mapM_ (unify t) (fmap (\(Qual _ x) -> x) psts)
  return $ concat $ fmap (\(Qual x _) -> x) psts

split :: [Tyvar] -> [Tyvar] -> [Pred] -> Infer ([Pred], [Pred])
split fs gs ps = do
  ce <- getClassEnv
  ps' <- Infer $ lift $ reduce ce ps
  let (ds, rs) = L.partition (all (`elem` fs) . getVars) ps'
  rs' <- defaultedPreds ce (fs ++ gs) rs
  return (ds, rs L.\\ rs')

inferExpl :: [Assump] -> Expl Lang -> Infer [Pred]
inferExpl as Expl{..} = do
  ce <- getClassEnv
  Qual qs t <- freshInst expl'type
  ps <- inferAlts as expl'alts t
  s <- getSubst
  let qs' = apply s qs
      t'  = apply s t
      fs  = getVars (apply s as)
      gs  = getVars t' S.\\ fs
      sc' = quantify (S.toList gs) (Qual qs' t')
      ps' = filter (not . entail ce qs') (apply s ps)
  (ds, rs) <- split (S.toList fs) (S.toList gs) ps'
  if expl'type /= sc'
    then throwError $ TypeError "Signature too general"
    else if not (null rs)
            then throwError $ TypeError "context too weak"
            else return ds


restricted :: [Impl a] -> Bool
restricted bs = any simple bs
  where
    simple Impl{..} = any (null . alt'pats) impl'alts

inferImpl :: [Assump] -> [Impl Lang] -> Infer ([Pred], [Assump])
inferImpl as bs = do
  ts <- mapM (const $ newTVar Star) bs
  let is    = fmap impl'name bs
      scs   = fmap toScheme ts
      as'   = zipWith (:>:) is scs ++ as
      altss = fmap impl'alts bs
  pss <- zipWithM (inferAlts as') altss ts
  s <- getSubst
  let ps'   = apply s (concat pss)
      ts'   = apply s ts
      fs    = getVars (apply s as)
      vss   = fmap getVars ts'
      gs    = L.foldr1 S.union vss S.\\ fs
  (ds, rs) <- split (S.toList fs) (S.toList $ foldr1 S.intersection vss) ps'
  if restricted bs
    then
      let gs'  = gs S.\\ getVars rs
          scs' = fmap (quantify (S.toList gs') . Qual []) ts'
       in return (ds ++ rs, zipWith (:>:) is scs')
    else
      let scs' = fmap (quantify (S.toList gs) . Qual []) ts'
      in  return (ds, zipWith (:>:) is scs')

inferPats :: [Pat] -> Infer ([Pred], [Assump], [Type])
inferPats pats = do
  psasts <- mapM inferPat pats
  let ps = concat $ fmap (\(p, _, _) -> p) psasts
      as = concat $ fmap (\(_, a, _) -> a) psasts
      ts = fmap (\(_, _, t) -> t) psasts
  return (ps, as, ts)

inferPat :: Pat -> Infer ([Pred], [Assump], Type)
inferPat = \case
  PVar idx -> do
    v <- newTVar Star
    return ([], [idx :>: toScheme v], v)
  PWildcard -> do
    v <- newTVar Star
    return ([], [], v)
  PLit lit -> do
    (Qual ps t) <- inferPrim lit
    return (ps, [], t)


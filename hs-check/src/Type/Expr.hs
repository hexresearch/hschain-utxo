module Type.Expr where

import Control.Monad
import Control.Monad.Except

import Type.ClassEnv
import Type.Subst
import Type.Type
import Type.Infer

import qualified Data.List as L
import qualified Data.Set as S

type Program = [BindGroup]

data Literal
  = LitInt Integer
  | LitChar Char
  | LitRat Rational
  | LitStr String

inferLit :: Literal -> Infer (Qual Type)
inferLit = \case
  LitChar _ -> return $ Qual [] charT
  LitInt _  -> do
    v <- newTVar Star
    return $ Qual [IsIn "Num" v] v
  LitStr _  -> return $ Qual [] stringT
  LitRat _  -> do
    v <- newTVar Star
    return $ Qual [IsIn "Fractional" v] v

data Pat
  = PVar Id
  | PWildcard
  | PAs Id Pat
  | PLit Literal
  | PNpk Id Integer
  | PCon Assump [Pat]

inferPat :: Pat -> Infer ([Pred], [Assump], Type)
inferPat = \case
  PVar idx -> do
    v <- newTVar Star
    return ([], [idx :>: toScheme v], v)
  PWildcard -> do
    v <- newTVar Star
    return ([], [], v)
  PAs idx pat -> do
    (ps, as, t) <- inferPat pat
    return (ps, (idx :>: toScheme t) : as, t)
  PLit lit -> do
    (Qual ps t) <- inferLit lit
    return (ps, [], t)
  PNpk idx k -> do
    t <- newTVar Star
    return ([IsIn "Integral" t], [idx :>: toScheme t], t)
  PCon (idx :>: sc) pats -> do
    (ps, as, ts) <- inferPats pats
    t' <- newTVar Star
    Qual qs t <- freshInst sc
    unify t (foldr fn t' ts)
    return (ps ++ qs, as, t')

inferPats :: [Pat] -> Infer ([Pred], [Assump], [Type])
inferPats pats = do
  psasts <- mapM inferPat pats
  let ps = concat $ fmap (\(p, _, _) -> p) psasts
      as = concat $ fmap (\(_, a, _) -> a) psasts
      ts = fmap (\(_, _, t) -> t) psasts
  return (ps, as, ts)

data Expr
  = Var Id
  | Lit Literal
  | Const Assump
  | Ap Expr Expr
  | Let BindGroup Expr

lam :: Id -> Expr -> Expr
lam arg body = Let (BindGroup [] [[Impl "f" [([PVar arg], body)]]]) (Var "f")

data BindGroup = BindGroup
  { bindGroup'expl :: [Expl]
  , bindGroup'impl :: [[Impl]]
  }

data Expl = Expl
  { expl'name  :: Id
  , expl'type  :: Scheme
  , expl'alts  :: [Alt]
  }

data Impl = Impl
  { impl'name  :: Id
  , impl'alts  :: [Alt]
  }

inferExpr :: [Assump] -> Expr -> Infer (Qual Type)
inferExpr as = \case
  Var idx -> do
    sc <- findAssump idx as
    freshInst sc
  Const (idx :>: sc) -> freshInst sc
  Lit lit -> inferLit lit
  Ap e f -> do
    Qual ps te <- inferExpr as e
    Qual qs tf <- inferExpr as f
    t <- newTVar Star
    unify (tf `fn` t) te
    return $ Qual (ps ++ qs) t
  Let bg e -> do
    (ps, as') <- inferBindGroup as bg
    Qual qs t <- inferExpr (as' ++ as) e
    return $ Qual (ps ++ qs) t

type Alt = ([Pat], Expr)

inferAlt :: [Assump] -> Alt -> Infer (Qual Type)
inferAlt as (pats, e) = do
  (ps, as', ts) <- inferPats pats
  Qual qs t <- inferExpr (as' ++ as) e
  return $ Qual (ps ++ qs) (foldr fn t ts)

inferAlts :: [Assump] -> [Alt] -> Type -> Infer [Pred]
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

inferExpl :: [Assump] -> Expl -> Infer [Pred]
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


restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where
    simple Impl{..} = any (null . fst) impl'alts

inferImpl :: [Assump] -> [Impl] -> Infer ([Pred], [Assump])
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
  return undefined
  if restricted bs
    then
      let gs'  = gs S.\\ getVars rs
          scs' = fmap (quantify (S.toList gs') . Qual []) ts'
       in return (ds ++ rs, zipWith (:>:) is scs')
    else
      let scs' = fmap (quantify (S.toList gs) . Qual []) ts'
      in  return (ds, zipWith (:>:) is scs')

inferBindGroup :: [Assump] -> BindGroup -> Infer ([Pred], [Assump])
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



module Type.Expr where

import Control.Monad
import Control.Monad.Except

import Type.ClassEnv
import Type.Subst
import Type.Type
import Type.Loc
import Type.Infer

import qualified Data.List as L
import qualified Data.Set as S

type Program = [BindGroup]

data Literal
  = LitInt Loc Integer
  | LitChar Loc Char
  | LitRat Loc Rational
  | LitStr Loc String

inferLit :: Literal -> Infer (Qual Type)
inferLit = \case
  LitChar loc _ -> return $ Qual loc [] (charT' loc)
  LitInt loc _  -> do
    v <- newTVar (Star loc)
    return $ Qual loc [IsIn loc (Id loc "Num") v] v
  LitStr loc _  -> return $ Qual loc [] (stringT' loc)
  LitRat loc _  -> do
    v <- newTVar (Star loc)
    return $ Qual loc [IsIn loc (Id loc "Fractional") v] v

data Pat
  = PVar Loc Id
  | PWildcard Loc
  | PAs Loc Id Pat
  | PLit Loc Literal
  | PNpk Loc Id Integer
  | PCon Loc Assump [Pat]

inferPat :: Pat -> Infer ([Pred], [Assump], Type)
inferPat = \case
  PVar loc idx -> do
    v <- newTVar (Star loc)
    return ([], [idx :>: toScheme v], v)
  PWildcard loc -> do
    v <- newTVar (Star loc)
    return ([], [], v)
  PAs loc idx pat -> do
    (ps, as, t) <- inferPat pat
    return (ps, (idx :>: toScheme t) : as, t)
  PLit loc lit -> do
    (Qual _ ps t) <- inferLit lit
    return (ps, [], t)
  PNpk loc idx k -> do
    t <- newTVar (Star loc)
    return ([IsIn loc "Integral" t], [idx :>: toScheme t], t)
  PCon loc (idx :>: sc) pats -> do
    (ps, as, ts) <- inferPats pats
    t' <- newTVar (Star loc)
    Qual _ qs t <- freshInst sc
    unify t (foldr (fn' loc) t' ts)
    return (ps ++ qs, as, t')

inferPats :: [Pat] -> Infer ([Pred], [Assump], [Type])
inferPats pats = do
  psasts <- mapM inferPat pats
  let ps = concat $ fmap (\(p, _, _) -> p) psasts
      as = concat $ fmap (\(_, a, _) -> a) psasts
      ts = fmap (\(_, _, t) -> t) psasts
  return (ps, as, ts)

data Expr
  = Var Loc Id
  | Lit Loc Literal
  | Const Loc Assump
  | Ap Loc Expr Expr
  | Let Loc BindGroup Expr

instance HasLoc Expr where
  getLoc = \case
    Var loc _ -> loc
    Lit loc _ -> loc
    Const loc _ -> loc
    Ap loc _ _ -> loc
    Let loc _ _ -> loc

lam :: Id -> Expr -> Expr
lam arg body = Let (getLoc arg) (BindGroup [] [[Impl "f" [(Alt (getLoc arg) [PVar (getLoc arg) arg] body)]]]) (Var (getLoc body) "f")

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

instance HasLoc Impl where
  getLoc = getLoc . impl'name

instance HasLoc Expl where
  getLoc = getLoc . expl'name

inferExpr :: [Assump] -> Expr -> Infer (Qual Type)
inferExpr as = \case
  Var _ idx -> do
    sc <- findAssump idx as
    freshInst sc
  Const _ (idx :>: sc) -> freshInst sc
  Lit _ lit -> inferLit lit
  Ap loc e f -> do
    Qual _ ps te <- inferExpr as e
    Qual _ qs tf <- inferExpr as f
    t <- newTVar (Star loc)
    unify (fn' loc tf t) te
    return $ Qual loc (ps ++ qs) t
  Let loc bg e -> do
    (ps, as') <- inferBindGroup as bg
    Qual _ qs t <- inferExpr (as' ++ as) e
    return $ Qual loc (ps ++ qs) t

data Alt = Alt
  { alt'loc   :: Loc
  , alt'pats  :: [Pat]
  , alt'expr  :: Expr }

inferAlt :: [Assump] -> Alt -> Infer (Qual Type)
inferAlt as (Alt loc pats e) = do
  (ps, as', ts) <- inferPats pats
  Qual loc' qs t <- inferExpr (as' ++ as) e
  return $ Qual loc' (ps ++ qs) (foldr (fn' loc') t ts)

inferAlts :: [Assump] -> [Alt] -> Type -> Infer [Pred]
inferAlts as alts t = do
  psts <- mapM (inferAlt as) alts
  mapM_ (unify t) (fmap (\(Qual _ _ x) -> x) psts)
  return $ concat $ fmap (\(Qual _ x _) -> x) psts

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
  Qual loc qs t <- freshInst expl'type
  ps <- inferAlts as expl'alts t
  s <- getSubst
  let qs' = apply s qs
      t'  = apply s t
      fs  = getVars (apply s as)
      gs  = getVars t' S.\\ fs
      sc' = quantify (S.toList gs) (Qual loc qs' t')
      ps' = filter (not . entail ce qs') (apply s ps)
  (ds, rs) <- split (S.toList fs) (S.toList gs) ps'
  if expl'type /= sc'
    then throwError $ singleTypeError loc "Signature too general"
    else if not (null rs)
            then throwError $ singleTypeError loc "context too weak"
            else return ds


restricted :: [Impl] -> Bool
restricted bs = any simple bs
  where
    simple Impl{..} = any (null . alt'pats) impl'alts

inferImpl :: [Assump] -> [Impl] -> Infer ([Pred], [Assump])
inferImpl as bs = do
  ts <- mapM (newTVar . Star . getLoc) bs
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
          scs' = fmap (quantify (S.toList gs') . (\t -> Qual (getLoc t) [] t)) ts'
       in return (ds ++ rs, zipWith (:>:) is scs')
    else
      let scs' = fmap (quantify (S.toList gs) . (\t -> Qual (getLoc t) [] t)) ts'
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



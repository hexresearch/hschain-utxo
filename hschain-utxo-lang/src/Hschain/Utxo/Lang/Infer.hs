module Hschain.Utxo.Lang.Infer where

import Control.Monad.Except
import Control.Monad.Trans
import Data.Fix hiding ((~>))
import Data.Vector (Vector)

import Type.ClassEnv
import Type.Infer
import Type.Loc
import Type.Subst
import Type.Type
import Type.Pretty

import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr

import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Set as S
import qualified Data.Vector as V

import Debug.Trace

infixr ~>

(~>) = fn

inferPrim :: Loc -> Prim -> Infer (Qual Type)
inferPrim loc = \case
  PrimInt _     -> numT loc
  PrimDouble _  -> numT loc
  PrimString _  -> return $ Qual loc [] (textT' loc)
  PrimBool _    -> return $ Qual loc [] (boolT' loc)
  where
    numT loc = do
      v <- newTVar (Star loc)
      return $ Qual loc [IsIn loc (Id loc "Num") v] v

inferExpr :: [Assump] -> Lang -> Infer (Qual Type)
inferExpr as (Fix x) = case x of
  Var loc varName        -> fromVar loc as varName
  Apply loc a b          -> fromApply loc as a b
  InfixApply loc a v b   -> inferExpr as $ unfoldInfixApply loc a v b
  Lam loc varName a      -> fromLam loc as varName a
  LamList loc vars a     -> fromLamList loc as vars a
  Let loc varName a      -> fromLet loc as varName a
  LetRec loc varName a b -> fromLetRec loc as varName a b
  Ascr loc a ty          -> fromAscr loc as a ty
  -- primitives
  PrimE loc p            -> fromPrim loc p
  -- logic
  If loc a b c           -> fromIf loc as a b c
  Pk loc a               -> fromPk loc as a
  -- tuples
  Tuple loc t            -> fromTuple loc as t
  -- operations
  UnOpE loc op a         -> fromUnOp loc as op a
  BinOpE loc op a b      -> fromBinOp loc as op a b
  -- environment
  GetEnv loc envId       -> fromGetEnv loc as envId
  -- vectors
  VecE loc vec           -> fromVec loc as vec
  -- text
  TextE loc txt          -> fromText loc as txt
  -- boxes
  BoxE loc box           -> fromBox loc as box
  -- undef value
  Undef loc              -> fromUndef loc
  -- debug
  Trace loc a b          -> fromTrace loc as a b

fromVar :: Loc -> [Assump] -> VarName -> Infer (Qual Type)
fromVar loc as v = do
  sc <- findAssump (fromVarName v) as
  freshInst sc

fromApply :: Loc -> [Assump] -> Lang -> Lang -> Infer (Qual Type)
fromApply loc as f a = do
  Qual _ pf tf <- inferExpr as f
  Qual _ pa ta <- inferExpr as a
  t <- newTVar (Star loc)
  unify (ta `fn` t) tf
  return $ Qual loc (pf ++ pa) t

fromLam :: Loc -> [Assump] -> VarName -> Lang -> Infer (Qual Type)
fromLam loc as v b = inferExpr as $ lam v b
  where
    -- toLet :: VarName -> Lang -> Lang
    -- toLet arg body = singleLet loc (VarName loc "f") (Fix $ Lam loc (VarName loc "x") body) (Fix $ Var loc (VarName loc "f"))

    lam :: VarName -> Lang -> Lang
    lam arg body = Fix $ Let (getLoc arg) (BindGroup [] [[Impl "f" [(Alt [PVar (getLoc arg) (fromVarName arg)] body)]]]) (Fix $ Var (getLoc body) "f")

fromLamList :: Loc -> [Assump] -> [VarName] -> Lang -> Infer (Qual Type)
fromLamList loc as args body = inferExpr as $ unfoldLamList loc args body

fromLet loc as bg e = do
  (ps, as') <- inferBindGroup as bg
  Qual _ qs t <- inferExpr (as' ++ as) e
  return $ Qual loc (ps ++ qs) t

fromLetRec loc = undefined

fromAscr loc as a t = do
  Qual _ aPs aT <- inferExpr as a
  unify aT t
  return $ Qual loc aPs t

fromPrim :: Loc -> Prim -> Infer (Qual Type)
fromPrim loc = inferPrim loc

fromIf :: Loc -> [Assump] -> Lang -> Lang -> Lang -> Infer (Qual Type)
fromIf loc as cond a b = do
  Qual _ condPs condT <- inferExpr as cond
  Qual _ aPs aT <- inferExpr as a
  Qual _ bPs bT <- inferExpr as b
  unify condT boolT
  unify aT bT
  return $ Qual loc (condPs ++ aPs ++ bPs) aT

fromPk :: Loc -> [Assump] -> Lang -> Infer (Qual Type)
fromPk loc as a = do
  Qual _ aPs aT <- inferExpr as a
  unify aT textT
  return $ Qual loc aPs textT

fromTuple :: Loc -> [Assump] -> Vector Lang -> Infer (Qual Type)
fromTuple loc as vs = do
      ts <- mapM (newTVar . Star . getLoc) vs
      qs <- mapM (inferExpr as) vs
      let ts' = fmap (\(Qual _ _ t) -> t) qs
          ps  = fmap (\(Qual _ p _) -> p) qs
      zipWithM unify (V.toList ts) (V.toList ts')
      return $ Qual loc (concat $ V.toList ps) (tupleT' loc $ V.toList ts')

fromVec :: Loc -> [Assump] -> VecExpr Lang -> Infer (Qual Type)
fromVec _ as = \case
  NewVec loc vs -> do
    t <- newTVar (Star loc)
    ps <- fmap concat $ mapM ((\(Qual _ ps ty) -> unify t ty >> return ps) <=< inferExpr as) vs
    return $ Qual loc ps $ vectorT t
  VecAppend loc a b -> do
    Qual _ aPs aT <- inferExpr as a
    Qual _ bPs bT <- inferExpr as b
    t <- newTVar (Star loc)
    unify (vectorT t) aT
    unify (vectorT t) bT
    return $ Qual loc (aPs ++ bPs) (vectorT t)
  VecAt loc a b -> do
    t <- newTVar (Star loc)
    Qual _ aPs aT <- inferExpr as a
    Qual _ bPs bT <- inferExpr as b
    unify (vectorT t) aT
    unify bT intT
    return $ Qual loc aPs t
  VecLength loc -> do
    t <- newTVar (Star loc)
    return $ Qual loc [] (t ~> intT)
  VecMap loc -> do
    a <- newTVar (Star loc)
    b <- newTVar (Star loc)
    return $ Qual loc [] ( (a ~> b) ~> vectorT a ~> vectorT b)
  VecFold loc    -> do
    a <- newTVar (Star loc)
    b <- newTVar (Star loc)
    return $ Qual loc [] ( (a ~> b ~> a) ~> a ~> vectorT b ~> a)

fromUnOp :: Loc -> [Assump] -> UnOp -> Lang -> Infer (Qual Type)
fromUnOp loc as = \case
  Not       -> fromNot
  Neg       -> fromNeg
  TupleAt n -> fromTupleAt n
  where
    fromNot a = do
      Qual _ _ aT <- inferExpr as a
      unify aT boolT
      return $ Qual loc [] (boolT' loc)

    fromNeg a = do
      t <- newTVar (Star loc)
      Qual _ aPs aT <- inferExpr as a
      unify t aT
      return $ Qual loc ([IsIn loc (Id loc "Num") t] ++ aPs) t

    fromTupleAt n vs = do
      Qual _ ps ts <- inferExpr as vs
      extractTuple n ps ts
      where
        extractTuple n ps ts = case (V.fromList $ tupleArgs ts) V.!? n of
          Just t   -> return $ Qual loc ps t
          Nothing  -> throwError $ singleTypeError loc "Wrong size tuple"

        tupleArgs = reverse . go []
          where
            go res ts = case ts of
              TAp _ a b -> go (b : res) a
              _         -> res

fromBinOp :: Loc -> [Assump] -> BinOp -> Lang -> Lang -> Infer (Qual Type)
fromBinOp loc as = \case
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
      Qual _ aPs aT <- inferExpr as a
      Qual _ bPs bT <- inferExpr as b
      unify aT boolT
      unify bT boolT
      return $ Qual loc [] (boolT' loc)

    numOp a b = do
      t <- newTVar (Star loc)
      Qual _ aPs aT <- inferExpr as a
      Qual _ bPs bT <- inferExpr as b
      unify t aT
      unify t bT
      unify aT bT
      return $ Qual loc ((IsIn loc (Id loc "Num") t) : (aPs ++ bPs)) t

    eqOp a b = do
      t <- newTVar (Star loc)
      Qual _ aPs aT <- inferExpr as a
      Qual _ bPs bT <- inferExpr as b
      unify t aT
      unify t bT
      return $ Qual loc [] (boolT' loc)

    ordOp a b = do
      t <- newTVar (Star loc)
      Qual _ aPs aT <- inferExpr as a
      Qual _ bPs bT <- inferExpr as b
      unify t aT
      unify t bT
      return $ Qual loc [] (boolT' loc)

    funOp f g = do
      Qual _ fPs fT <- inferExpr as f
      Qual _ gPs gT <- inferExpr as g
      aT <- newTVar (Star loc)
      bT <- newTVar (Star loc)
      cT <- newTVar (Star loc)
      unify fT (bT ~> cT)
      unify gT (aT ~> bT)
      return $ Qual loc (fPs ++ gPs) ((bT ~> cT) ~> (aT ~> bT) ~> aT ~> cT)

fromGetEnv :: Loc -> [Assump] -> EnvId Lang -> Infer (Qual Type)
fromGetEnv _ as = \case
  Height loc      -> return $ Qual loc [] (intT' loc)
  Input loc a     -> fromBoxGet loc a
  Output loc a    -> fromBoxGet loc a
  Self loc        -> return $ Qual loc [] (boxT' loc)
  Inputs loc      -> vecBox loc
  Outputs loc     -> vecBox loc
  GetVar loc a    -> fromGetVar loc a
  where
    vecBox loc = return $ Qual loc [] $ vectorT' loc (boxT' loc)

    fromBoxGet loc a = do
      Qual _ _ aT <- inferExpr as a
      unify aT intT
      return $ Qual loc [] (boxT' loc)

    fromGetVar loc arg = do
      t <- newTVar (Star loc)
      Qual _ _ argT <- inferExpr as arg
      unify argT textT
      return $ Qual loc [] t

fromText :: Loc -> [Assump] -> TextExpr Lang -> Infer (Qual Type)
fromText _ as = \case
  TextAppend loc a b -> do
    Qual _ _ aT <- inferExpr as a
    Qual _ _ bT <- inferExpr as b
    unify aT textT
    unify bT textT
    return $ Qual loc [] (textT' loc)
  -- todo: use text tag to restrict type inference here
  ConvertToText _ loc  -> do
    t <- newTVar (Star loc)
    return $ Qual loc [] $ fn' loc t (textT' loc)
  TextLength loc     -> return $ Qual loc [] $ fn' loc (textT' loc) (intT' loc)
  TextHash loc _     -> return $ Qual loc [] $ fn' loc (textT' loc) (textT' loc)

fromBox :: Loc -> [Assump] -> BoxExpr Lang -> Infer (Qual Type)
fromBox _ as = \case
  PrimBox loc _ -> return $ Qual loc [] (boxT' loc)
  BoxAt loc box field -> fromBoxField loc box field
  where
    fromBoxField loc box field = do
      Qual _ _boxPs bT <- inferExpr as box
      unify bT boxT
      case field of
        BoxFieldId      -> return $ Qual loc [] (textT' loc)
        BoxFieldValue   -> return $ Qual loc [] (doubleT' loc)
        BoxFieldScript  -> return $ Qual loc [] (scriptT' loc)
        BoxFieldArg txt -> do
          t <- newTVar (Star loc)
          Qual _ txtPs txtT <- inferExpr as txt
          unify txtT textT
          return $ Qual loc [] t


fromUndef :: Loc -> Infer (Qual Type)
fromUndef loc = fmap (Qual loc []) $ newTVar (Star loc)

fromTrace :: Loc -> [Assump] -> Lang -> Lang -> Infer (Qual Type)
fromTrace loc as msg arg = do
  Qual _ msgPs msgT <- inferExpr as msg
  Qual _ argPs argT <- inferExpr as arg
  unify msgT textT
  return $ Qual loc (msgPs ++ argPs) argT


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

inferModule :: ClassEnv -> [Assump] -> Module -> Either TypeError [Assump]
inferModule ce as (Module _ bgs) = (\inf -> runInfer inf ce) $ do
  (ps, as') <- inferSeq inferBindGroup as bgs
  s <- getSubst
  rs <- Infer $ lift $ reduce ce (apply s ps)
  s' <- defaultSubst ce [] rs
  return $ apply (s' `compose` s) as'

inferAlt :: [Assump] -> Alt Lang -> Infer (Qual Type)
inferAlt as alt@(Alt pats e) = do
  (ps, as', ts) <- inferPats pats
  Qual _ qs t <- inferExpr (as' ++ as) e
  return $ Qual (getLoc alt) (ps ++ qs) (foldr fn t ts)

inferAlts :: [Assump] -> [Alt Lang] -> Type -> Infer [Pred]
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

inferExpl :: [Assump] -> Expl Lang -> Infer [Pred]
inferExpl as expl@Expl{..} = do
  let loc = getLoc expl
  ce <- getClassEnv
  Qual _ qs t <- freshInst expl'type
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


restricted :: [Impl a] -> Bool
restricted bs = any simple bs
  where
    simple Impl{..} = any (null . alt'pats) impl'alts

inferImpl :: [Assump] -> [Impl Lang] -> Infer ([Pred], [Assump])
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
  if restricted bs
    then
      let gs'  = gs S.\\ getVars rs
          scs' = fmap (quantify (S.toList gs') . (\t -> Qual (getLoc t) [] t)) ts'
       in return (ds ++ rs, zipWith (:>:) is scs')
    else
      let scs' = fmap (quantify (S.toList gs) . (\t -> Qual (getLoc t) [] t)) ts'
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
  PVar loc idx -> do
    v <- newTVar (Star loc)
    return ([], [idx :>: toScheme v], v)
  {-
  PWildcard loc -> do
    v <- newTVar (Star loc)
    return ([], [], v)
  PLit loc lit -> do
    (Qual _ ps t) <- inferPrim lit
    return (ps, [], t)
  -}

-------------------------------------------
--

runInferModule :: [Assump] -> Lang -> Either TypeError (Qual Type)
runInferModule = undefined

runInferExpr :: [Assump] -> Lang -> Either TypeError (Qual Type)
runInferExpr as expr = do
  ce <- defaultClassEnv
  runInfer (infer ce) ce
  where
    infer ce = do
      (Qual loc ps exprT) <- inferExpr as expr
      s <- getSubst
      rs <- Infer $ lift $ reduce ce (apply s ps)
      s' <- defaultSubst ce [] rs
      let mainSubst = s' `compose` s
          resExprT = apply mainSubst exprT
          normS = normalizeSubst resExprT
      return $ Qual loc (removeCons $ apply (normS `compose` mainSubst) rs) $ apply normS resExprT

    tr x = trace (T.unpack $ pp x) x

defaultClassEnv :: Either TypeError ClassEnv
defaultClassEnv = addCoreClasses initialEnv

addCoreClasses :: EnvTransformer
addCoreClasses =
      addClass "Eq" [] eqInsts
  <:> addClass "Ord" ["Eq"] ordInsts
  <:> addClass "Show" [] showInsts
  <:> addClass "Num" [] numInsts
  where
    eqInsts = instBy eq
      where eq = qual "Eq"

    ordInsts = instBy ord
      where ord = qual "Ord"

    instBy f = fmap f [boolT, textT, intT, doubleT]

    numInsts = fmap num [intT, doubleT]
      where num = qual "Num"

    showInsts = fmap sh [intT, doubleT, textT, boolT]
      where sh = qual "Show"

    qual idx ty = Qual noLoc [] (IsIn noLoc idx ty)




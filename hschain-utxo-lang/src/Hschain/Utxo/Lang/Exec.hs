-- | Module defines reduction of the expression (expression execution).
--
-- We reduce expression to the primitive value.
-- For blockchain transaction verification it is going
-- to be sigma-expression.
--
-- For now it is done with simple algorithm of substitution of
-- values (application of lambda abstractions and substitution of subexpressions).
module Hschain.Utxo.Lang.Exec(
    execLang
  , evalModule
  , runExec
  , Error(..)
  , BoolExprResult(..)
) where

import Hex.Common.Control
import Hex.Common.Text

import Codec.Serialise

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict
import Control.Monad.Extra (firstJustM)

import Data.ByteString (ByteString)
import Data.Fix
import Data.Int
import Data.Map.Strict (Map)
import Data.String
import Data.Text (Text)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Build()
import Hschain.Utxo.Lang.Desugar
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Monad
import Hschain.Utxo.Lang.Exec.Module
import Hschain.Utxo.Lang.Exec.Subst
import Hschain.Utxo.Lang.Sigma (Sigma, PublicKey, notSigma, publicKeyFromText)
import Hschain.Utxo.Lang.Utils.ByteString

import qualified Data.ByteString.Lazy as LB
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Language.HM as H

import qualified Hschain.Utxo.Lang.Sigma as S

{- for debug
import Debug.Trace
import Hschain.Utxo.Lang.Pretty
import Text.Show.Pretty (ppShow)

trace' :: Show a => a -> a
trace' x = trace (ppShow x) x

trace2 :: Lang -> a -> a
trace2 expr x = trace (T.unpack $ renderText expr) x
-}

-- | Context of execution
data Ctx = Ctx
  { ctx'vars       :: !(Map VarName Lang)  -- ^ global bindings (outer scope)
  , ctx'userArgs   :: !Args                -- ^ list user arguments for transaction
  , ctx'height     :: !Int64               -- ^ height of blockchain
  , ctx'inputs     :: !(Vector Box)        -- ^ vector of input boxes
  , ctx'outputs    :: !(Vector Box)        -- ^ vector of ouptut boxes
  , ctx'debug      :: !Text                -- ^ debug log for executed expression
  , ctx'freshVarId :: !Int                 -- ^ counter for allocation of fresh variables
  }

-- | Execution monad.
newtype Exec a = Exec (StateT Ctx (Either Error) a)
  deriving newtype (MonadState Ctx, Monad, Functor, Applicative, MonadError Error)

instance Alternative Exec where
  empty = throwError $ ExecError $ Undefined noLoc
  (Exec stA) <|> (Exec stB) =
    Exec $ StateT $ \s ->
      let eRes = runStateT stA s
      in  case eRes of
            Right res -> return res
            Left _    -> runStateT stB s

getHeight :: Exec Int64
getHeight = fmap ctx'height get

getInputs :: Exec (Vector Box)
getInputs = fmap ctx'inputs get

getOutputs :: Exec (Vector Box)
getOutputs = fmap ctx'outputs get

instance MonadFreshVar Exec where
  getFreshVarName = do
    idx <- fmap ctx'freshVarId get
    modify' $ \st -> st { ctx'freshVarId = ctx'freshVarId st + 1 }
    return $ toName idx
    where
      toName n = fromString $ '$' : show n

instance MonadLang Exec where

getUserArgs :: Exec Args
getUserArgs = fmap  ctx'userArgs get

saveTrace :: Text -> Exec ()
saveTrace msg =
  modify' $ \st -> st { ctx'debug = T.unlines [ctx'debug st, msg] }

-- | Run execution monad.
runExec :: ExecCtx -> Args -> Int64 -> Vector Box -> Vector Box -> Exec a -> Either Error (a, Text)
runExec (ExecCtx binds) args height inputs outputs (Exec st) =
  fmap (second ctx'debug) $ runStateT st emptyCtx
  where
    emptyCtx = Ctx binds args height inputs outputs mempty 0

-- | Performs execution of expression.
execLang :: Lang -> Exec Lang
execLang (Fix topExpr) = case topExpr of
    Var _ name -> getVar name
    PrimE loc p  -> pure $ Fix $ PrimE loc p
    Tuple loc as -> Fix . Tuple loc <$> mapM rec as
    Ascr loc a t -> fmap (\x -> Fix $ Ascr loc x t) $ rec a
    Cons loc name vs -> fromCons loc name vs
    -- case of
    CaseOf loc v xs -> fromCaseOf loc v xs
    -- records
    RecConstr _ _ _ -> thisShouldNotHappen (Fix topExpr)
    RecUpdate loc a upds -> fromRecUpdate loc a upds
    -- operations
    UnOpE loc uo x -> fromUnOp loc uo x
    BinOpE loc bi a b -> fromBiOp loc bi a b
    Apply loc a b -> fromApply loc a b
    InfixApply loc a v b -> fromInfixApply loc a v b
    Lam loc pat b -> fromLam loc pat b
    LamList loc vars a -> fromLamList loc vars a
    Let loc varName a -> fromLet loc varName a
    PrimLet _ _ _ -> error "Undefined exec for PrimLet case"
    -- logic
    If loc a b c -> fromIf loc a b c
    -- environment
    GetEnv loc idx -> fromEnv loc idx
    BoxE loc box -> fromBoxExpr loc box
    SigmaE loc sigma -> fromSigma loc sigma
    VecE loc vec -> fromVec loc vec
    TextE loc txt -> fromText loc txt
    BytesE loc bs -> fromBytes loc bs
    Trace loc str a -> fromTrace loc str a
    AltE loc a b -> rec a <|> rec b <|>  (throwError $ ExecError $ Undefined loc)
    FailCase loc -> throwError $ ExecError $ Undefined loc
  where
    rec = execLang

    getVar :: VarName -> Exec Lang
    getVar name = do
      vars <- fmap ctx'vars get
      case M.lookup name vars of
        Just res -> return res
        Nothing  -> Exec $ lift $ Left $ ExecError $ UnboundVariables [name]

    fromCons loc name vs = fmap (Fix . Cons loc name) $ mapM rec vs

    fromCaseOf loc v xs = do
      res <- rec v
      maybe err rec =<< firstJustM (matchCase res) xs
      where
        err = nonExaustiveCase loc $ Fix $ CaseOf loc v xs

        matchCase :: Lang -> CaseExpr Lang -> Exec (Maybe Lang)
        matchCase expr CaseExpr{..} =
          case caseExpr'lhs of
            PVar _ pv             -> onVar pv
            PPrim _ p             -> onPrim p
            PCons _ consName pats -> onCons consName pats
            PTuple _ pats         -> onTuple pats
            PWildCard _           -> onWildCard
          where
            onVar pv  = return $ Just $ singleLet (H.getLoc pv) pv expr caseExpr'rhs

            onPrim p = return $ matchPrim p expr caseExpr'rhs

            onCons consName pats = do
              (vs, rhs') <- reduceSubPats pats caseExpr'rhs
              return $ matchCons consName expr vs rhs'

            onTuple pats = do
              (vs, rhs') <- reduceSubPats pats caseExpr'rhs
              return $ matchTuple expr vs rhs'

            onWildCard = return $ Just caseExpr'rhs

        matchPrim p (Fix expr) rhs =
          case expr of
            PrimE _ k | k == p -> Just rhs
            _                  -> Nothing

        matchCons consName (Fix expr) vs rhs =
          case expr of
            Cons src exprName args ->
              if exprName == consName
                then Just $ Fix $ Let src (zipWith simpleBind vs (V.toList args)) rhs
                else Nothing
            _ -> Nothing

        matchTuple (Fix expr) vs rhs =
          case expr of
            Tuple src args -> Just $ Fix $ Let src (zipWith simpleBind vs (V.toList args)) rhs
            _              -> Nothing

    fromRecUpdate _ a upds = rec $ foldl go a upds
      where
        go res (field, val) = desugarRecordUpdate field val res



    fromUnOp _ uo x = do
      x' <- rec x
      case uo of
        Not         -> fromNot x'
        Neg         -> fromNeg x'
        TupleAt _ n -> fromTupleAt n x'
      where
        fromNot expr = case expr of
          Fix (PrimE loc1 (PrimBool b))  -> prim loc1 $ PrimBool $ not b
          Fix (PrimE loc1 (PrimSigma b)) -> prim loc1 $ either PrimBool PrimSigma $ notSigma b
          _                                   -> thisShouldNotHappen x

        fromNeg expr = case expr of
          Fix (PrimE loc1 a) -> case a of
            PrimInt n    -> prim loc1 $ PrimInt $ negate n
            other        -> thisShouldNotHappen $ Fix $ PrimE loc1 other
          _                  -> thisShouldNotHappen x

        fromTupleAt n expr = case expr of
          Fix (Tuple _ as) -> maybe (outOfBound x) return $ as V.!? n
          _                -> thisShouldNotHappen x

    fromBiOp loc bi x y = do
      a <- rec x
      b <- rec y
      case bi of
        And       -> fromAnd loc a b
        Or        -> fromOr loc a b
        Plus      -> fromPlus loc a b
        Minus     -> fromMinus loc a b
        Times     -> fromTimes loc a b
        Div       -> fromDiv loc a b
        Equals    -> fromEq loc a b
        NotEquals -> fromNotEq loc a b
        LessThan  -> fromLt loc a b
        GreaterThan -> fromGt loc a b
        LessThanEquals -> fromLte loc a b
        GreaterThanEquals -> fromGte loc a b

    fromAnd, fromOr :: Loc -> Lang -> Lang -> Exec Lang

    -- todo: maybe it's worth to make it lazy
    fromAnd loc x y = do
      Fix x' <- rec x
      Fix y' <- rec y
      case (x', y') of
        (PrimE locX1 (PrimBool a), _) ->
          if a
            then return (Fix y')
            else prim locX1 $ PrimBool False
        (_, PrimE locX1 (PrimBool a)) ->
          if a
            then return (Fix x')
            else prim locX1 $ PrimBool False
        (PrimE _ (PrimSigma a), PrimE _ (PrimSigma b)) ->
          return $ Fix $ PrimE loc $ PrimSigma $ Fix $ S.SigmaAnd [a, b]
        _                 -> thisShouldNotHappen $ Fix $ BinOpE loc And x y

    -- todo: maybe it's worth to make it lazy
    fromOr loc x y = do
      Fix x' <- rec x
      Fix y' <- rec y
      case (x', y') of
        (PrimE locX1 (PrimBool a), _) ->
          if a
            then prim locX1 $ PrimBool True
            else return (Fix y')
        (_, PrimE locX1 (PrimBool a)) ->
          if a
            then prim locX1 $ PrimBool True
            else return (Fix x')
        (PrimE _ (PrimSigma a), PrimE _ (PrimSigma b)) ->
          return $ Fix $ PrimE loc $ PrimSigma $ Fix $ S.SigmaOr [a, b]
        _                 -> thisShouldNotHappen $ Fix $ BinOpE loc And x y

    fromPlus  loc = fromNumOp2 loc Plus  (NumOp2 (+))
    fromMinus loc = fromNumOp2 loc Minus (NumOp2 (\x y -> x - y))
    fromTimes loc = fromNumOp2 loc Times (NumOp2 (*))
    fromDiv   loc = fromNumOp2 loc Div   (NumOp2 div)

    fromNumOp2 loc op NumOp2{..} (Fix x) (Fix y) = case (x, y) of
      (PrimE locA1 a, PrimE _ b) -> case (a, b) of
        (PrimInt m, PrimInt n) -> prim locA1 $ PrimInt $ numOp2'int m n
        _ -> err
      _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc op (Fix x) (Fix y)


    fromEq loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim locA1 $ PrimBool $ a1 == a2
          (PrimInt a1, PrimInt a2)       -> prim locA1 $ PrimBool $ a1 == a2
          (PrimString a1, PrimString a2) -> prim locA1 $ PrimBool $ a1 == a2
        -- todo: we have to decide on mixed num types
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc Equals (Fix x) (Fix y)

    fromNotEq loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim locA1 $ PrimBool $ a1 /= a2
          (PrimInt a1, PrimInt a2)       -> prim locA1 $ PrimBool $ a1 /= a2
          (PrimString a1, PrimString a2) -> prim locA1 $ PrimBool $ a1 /= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc NotEquals (Fix x) (Fix y)

    fromLt loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim locA1 $ PrimBool $ a1 < a2
          (PrimInt a1, PrimInt a2)       -> prim locA1 $ PrimBool $ a1 < a2
          (PrimString a1, PrimString a2) -> prim locA1 $ PrimBool $ a1 < a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc LessThan (Fix x) (Fix y)

    fromGt loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim locA1 $ PrimBool $ a1 > a2
          (PrimInt a1, PrimInt a2)       -> prim locA1 $ PrimBool $ a1 > a2
          (PrimString a1, PrimString a2) -> prim locA1 $ PrimBool $ a1 > a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc GreaterThan (Fix x) (Fix y)

    fromLte loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim locA1 $ PrimBool $ a1 <= a2
          (PrimInt a1, PrimInt a2)       -> prim locA1 $ PrimBool $ a1 <= a2
          (PrimString a1, PrimString a2) -> prim locA1 $ PrimBool $ a1 <= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc LessThanEquals (Fix x) (Fix y)

    fromGte loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim locA1 $ PrimBool $ a1 >= a2
          (PrimInt a1, PrimInt a2)       -> prim locA1 $ PrimBool $ a1 >= a2
          (PrimString a1, PrimString a2) -> prim locA1 $ PrimBool $ a1 >= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc GreaterThanEquals (Fix x) (Fix y)

    fromIf loc c t e  = do
      Fix c' <- rec c
      case c' of
        PrimE _ (PrimBool b) -> if b then rec t else rec e
        _                      -> err
      where
        err = thisShouldNotHappen $ Fix $ If loc c t e

    -- we transform generic patterns in lambdas to simple lambdas with case-expressions.
    fromLam loc pat b = case pat of
      PVar _ _ -> return $ Fix $ Lam loc pat b
      _        -> do
        v <- getFreshVar loc
        return $ Fix $ Lam loc (PVar loc v) $ Fix $ CaseOf loc (Fix $ Var loc v) [CaseExpr pat b]

    fromLamList loc vars a = rec $ unfoldLamList loc vars a

    fromTrace _ str a = do
      Fix str' <- rec str
      case str' of
        PrimE _ (PrimString msg) -> do
          saveTrace msg
          rec a
        _ -> thisShouldNotHappen $ Fix str'

    fromPk loc x = do
      Fix x' <- rec x
      case x' of
        PrimE _ (PrimString pkeyTxt) ->
          case publicKeyFromText pkeyTxt of
            Just pkey  -> return $ Fix $ PrimE loc $ PrimSigma $ Fix $ S.SigmaPk pkey
            Nothing    -> parseError loc $ mconcat ["Failed to convert parse public key from string: ", pkeyTxt]
        _                                    -> thisShouldNotHappen x

    fromInfixApply loc a v b = rec $ unfoldInfixApply loc a v b

    fromApply loc fun arg = case fun of
      Fix (Apply _ (Fix (VecE _ (VecMap _))) f) -> do
        Fix f' <- rec f
        Fix vec' <- rec arg
        let errVal = fun
        case vec' of
          VecE loc1 (NewVec loc2 vs) -> rec $ Fix $ VecE loc1 (NewVec loc2 $ fmap (Fix . Apply loc (Fix f')) vs)
          _ -> thisShouldNotHappen errVal
      Fix (Apply _ (Fix (Apply _ (Fix (VecE _ (VecFold _))) f)) z) -> do
        fun' <- rec f
        z'   <- rec z
        Fix vec' <- rec arg
        let errVal = fun
        case vec' of
          VecE _ (NewVec _ vs) -> rec $ V.foldl (\a b -> app2 fun' a b) z' vs
          _ -> thisShouldNotHappen errVal
      Fix (VecE _ (VecLength _)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim loc . PrimInt . fromIntegral) $ vecSize arg'
      Fix (TextE _ (TextLength _)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim loc . PrimInt . fromIntegral) $ textSize arg'
      Fix (TextE _ (ConvertToText _ _)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim loc . PrimString) $ convertToText arg'
      Fix (Cons src name vs) -> rec $ Fix $ Cons src name (mappend vs $ V.singleton arg)
      _ -> do
        Fix fun' <- rec fun
        case fun' of
          Lam _ (PVar _ varName) body -> do
            arg' <- rec arg
            rec $ subst body varName arg'
          Lam _ x body -> do
            v <- getFreshVar (H.getLoc x)
            arg' <- rec arg
            rec $ subst (desugarGenLamPattern v x body) v arg'
          LamList src vars a -> do
            f <- fromLamList src vars a
            fromApply src f arg
          VecE loc1 (VecMap loc2) -> do
            arg' <- rec arg
            return $ Fix $ Apply loc (Fix (VecE loc1 (VecMap loc2))) arg'
          VecE loc1 (VecFold loc2) -> do
            arg' <- rec arg
            return $ Fix $ Apply loc (Fix (VecE loc1 (VecFold loc2))) arg'
          Apply loc1 (Fix (VecE loc2 (VecFold loc3))) a -> do
            a' <- rec a
            arg' <- rec arg
            return $ Fix $ Apply loc (Fix $ Apply loc1 (Fix (VecE loc2 (VecFold loc3))) a') arg'
          Cons src name vs -> rec $ Fix $ Cons src name (mappend vs $ V.singleton arg)
          other              -> throwError $ ExecError $ AppliedNonFunction $ Fix other

    desugarGenLamPattern newVar pat body =
      Fix $ CaseOf (H.getLoc newVar) (Fix $ Var (H.getLoc newVar) newVar)
        [CaseExpr pat body]

    fromLet _ = execDefs
      where
        execDefs ds e = case ds of
          [] -> rec e
          def:rest -> do
            let v = bind'name def
            body <- altGroupToExpr $ bind'alts def
            execDefs (fmap2 (\x -> subst x v body) rest) (subst e v body)

    fromEnv loc idx = do
      idx' <- mapM rec idx
      case idx' of
        Height _  -> prim loc . PrimInt . fromIntegral =<< getHeight
        Input loc1 (Fix (PrimE _ (PrimInt n))) -> toBox loc1 n =<< getInputs
        Output loc1 (Fix (PrimE _ (PrimInt n))) -> toBox loc1 n =<< getOutputs
        Inputs loc1  -> fmap (toBoxes loc1) getInputs
        Outputs loc1 -> fmap (toBoxes loc1) getOutputs
        GetVar varLoc argType -> do
          args <- getUserArgs
          return $ case argType of
            IntArg   -> toArgField varLoc PrimInt  $ args'ints args
            TextArg  -> toArgField varLoc PrimString $ args'texts args
            BoolArg  -> toArgField varLoc PrimBool $ args'bools args
            BytesArg -> toArgField varLoc PrimBytes $ args'bytes args
        _ -> thisShouldNotHappen $ Fix $ GetEnv loc idx
      where
        toBox loc1 n v = maybe (outOfBound $ Fix $ GetEnv loc idx) (pure . Fix . BoxE loc1 . PrimBox loc1) $ v V.!? (fromIntegral n)
        toBoxes loc1 vs = Fix $ VecE loc $ NewVec loc $ fmap (Fix . BoxE loc1 . PrimBox loc1) vs

    fromBoxExpr :: Loc -> BoxExpr Lang -> Exec Lang
    fromBoxExpr loc x = do
      x' <- mapM rec x
      case x' of
        PrimBox loc1 box -> return $ Fix $ BoxE loc $ PrimBox loc1 box
        BoxAt loc1 (Fix (BoxE _ (PrimBox _ box))) field -> getBoxField loc1 box field
        _ -> thisShouldNotHappen $ Fix $ BoxE loc x

    toArgField :: Loc -> (a -> Prim) -> V.Vector a -> Lang
    toArgField loc primCons as = Fix $ VecE loc $ NewVec loc $ fmap (Fix . PrimE loc . primCons) as

    getBoxField :: Loc -> Box -> BoxField Lang -> Exec Lang
    getBoxField loc Box{..} field = case field of
      BoxFieldId         -> prim loc $ PrimString $ unBoxId box'id
      BoxFieldValue      -> prim loc $ PrimInt $ box'value
      BoxFieldScript     -> prim loc $ PrimBytes $ unScript $ box'script
      BoxFieldArgList ty -> return $ case ty of
        IntArg   -> toArgField loc PrimInt    $ args'ints box'args
        TextArg  -> toArgField loc PrimString $ args'texts box'args
        BoolArg  -> toArgField loc PrimBool   $ args'bools box'args
        BytesArg -> toArgField loc PrimBytes  $ args'bytes box'args

    fromSigma _ x = do
      x' <- mapM rec x
      case x' of
        Pk loc a         -> fromPk loc a
        SAnd loc a b     -> fromSigmaOp S.SigmaAnd loc a b
        SOr loc a b      -> fromSigmaOp S.SigmaOr loc a b
        SPrimBool loc a  -> fromSigmaBool loc a

    fromSigmaOp cons loc a b = do
      a' <- getPrimSigmaOrFail =<< rec a
      b' <- getPrimSigmaOrFail =<< rec b
      return $ Fix $ PrimE loc $ PrimSigma $ Fix $ cons [a', b']

    fromSigmaBool loc a = do
      a' <- getPrimBoolOrFail =<< rec a
      return $ Fix $ PrimE loc $ PrimSigma $ Fix $ S.SigmaBool a'

    fromVec loc x = do
      x' <- mapM rec x
      case x' of
        NewVec loc1 v        -> fmap (Fix . VecE loc . NewVec loc1) $ mapM rec v
        VecAppend loc1 a b   -> do
          a' <- rec a
          b' <- rec b
          return $ Fix $ VecE loc $ case (a', b') of
            (Fix (VecE _ (NewVec _ v1)), Fix (VecE _ (NewVec _ v2))) -> NewVec loc $ mappend v1 v2
            _ -> VecAppend loc1 a' b'
        VecAt loc1 v n       -> do
          Fix v' <- rec v
          Fix n' <- rec n
          let errVal = Fix $ VecE loc $ VecAt loc1 (Fix v') (Fix n')
          res <- case (v', n') of
            (VecE _ (NewVec _ vs), PrimE _ (PrimInt idx)) -> maybe (outOfBound errVal) return $ vs V.!? (fromIntegral idx)
            _ -> thisShouldNotHappen errVal
          rec res
        VecMap loc1 -> return $ Fix $ VecE loc $ VecMap loc1
        VecFold loc1 -> return $ Fix $ VecE loc $ VecFold loc1
        VecLength loc1 -> return $ Fix $ VecE loc $ VecLength loc1

    fromText loc x = do
      x' <- mapM rec x
      case x' of
        TextAppend _ a b -> do
          a' <- rec a
          b' <- rec b
          return $ Fix $ case (a', b') of
            (Fix (PrimE _ (PrimString t1)), Fix (PrimE _ (PrimString t2))) -> PrimE loc $ PrimString $ mappend t1 t2
            _                                                              -> TextE loc $ TextAppend loc a' b'
        ConvertToText loc1 tag  -> returnText $ ConvertToText loc1 tag
        TextLength loc1         -> returnText $ TextLength loc1
        where
          returnText = return . Fix . TextE loc

    fromBytes loc x = do
      x' <- mapM rec x
      case x' of
        BytesAppend _ a b -> do
          a' <- rec a
          b' <- rec b
          return $ Fix $ case (a', b') of
            (Fix (PrimE _ (PrimBytes t1)), Fix (PrimE _ (PrimBytes t2))) -> PrimE loc $ PrimBytes $ mappend t1 t2
            _                                                            -> BytesE loc $ BytesAppend loc a' b'
        SerialiseToBytes src typeTag a -> fromSerialiseToBytes src typeTag a
        DeserialiseFromBytes src typeTag a -> fromDeserialiseFromBytes src typeTag a
        BytesHash src algo a -> case algo of
          Sha256 -> do
            bs <- getPrimBytesOrFail =<< rec a
            return $ Fix $ PrimE src $ PrimBytes $ getSha256 bs


    fromSerialiseToBytes src typeTag a =
      case typeTag of
        IntArg   -> serialiseBy getPrimIntOrFail
        TextArg  -> serialiseBy getPrimTextOrFail
        BoolArg  -> serialiseBy getPrimBoolOrFail
        BytesArg -> serialiseBy getPrimBytesOrFail
      where
        serialiseBy :: Serialise a => (Lang -> Exec a) -> Exec Lang
        serialiseBy getter = do
          n <- getter =<< rec a
          return $ Fix $ PrimE src $ PrimBytes $ LB.toStrict $ serialise n


    fromDeserialiseFromBytes _loc _typeTag _a = undefined


    textSize (Fix x) = case x of
      PrimE _ (PrimString txt) -> Just $ T.length txt
      _                          -> Nothing

    vecSize (Fix x) = case x of
      VecE _ (NewVec _ xs)      -> Just $ V.length xs
      VecE _ (VecAppend _ a b)  -> liftA2 (+) (vecSize a) (vecSize b)
      _                         -> Nothing

    convertToText (Fix x) = case x of
      PrimE _ p  -> Just $ convertPrim p
      _          -> Nothing
      where
        convertPrim = \case
          PrimInt n       -> showt n
          PrimString t    -> t
          PrimBool b      -> showt b
          PrimSigma s     -> showt s
          PrimBytes bs    -> showt bs

prim :: Loc -> Prim -> Exec Lang
prim loc p = return $ Fix $ PrimE loc p

toError :: ExecError -> Exec a
toError = throwError . ExecError

thisShouldNotHappen :: Lang -> Exec a
thisShouldNotHappen = toError . ThisShouldNotHappen

outOfBound :: Lang -> Exec Lang
outOfBound = toError . OutOfBound

nonExaustiveCase :: Loc -> Lang -> Exec Lang
nonExaustiveCase loc expr = toError $ NonExaustiveCase loc expr

parseError :: Loc -> Text -> Exec Lang
parseError loc msg = throwError $ ParseError loc msg

type Mono2 a = a -> a -> a

data NumOp2 = NumOp2
  { numOp2'int    :: Mono2 Int64
  }


getPrim :: Lang -> Maybe (Loc, Prim)
getPrim (Fix a) = case a of
  PrimE loc p -> Just (loc, p)
  _           -> Nothing

getPrimSigma :: Prim -> Maybe (Sigma PublicKey)
getPrimSigma = \case
  PrimSigma p -> Just p
  _           -> Nothing

getPrimBool :: Prim -> Maybe Bool
getPrimBool = \case
  PrimBool b -> Just b
  _          -> Nothing

getPrimInt :: Prim -> Maybe Int64
getPrimInt = \case
  PrimInt b -> Just b
  _         -> Nothing

getPrimText :: Prim -> Maybe Text
getPrimText = \case
  PrimString b -> Just b
  _            -> Nothing

getPrimBytes :: Prim -> Maybe ByteString
getPrimBytes = \case
  PrimBytes b -> Just b
  _           -> Nothing

getPrimSigmaOrFail :: Lang -> Exec (Sigma PublicKey)
getPrimSigmaOrFail x = maybe (thisShouldNotHappen x) pure $
  getPrim x >>= (\(_, p) -> getPrimSigma p)

getPrimOrFailBy :: (Prim -> Maybe a) -> Lang -> Exec a
getPrimOrFailBy getter x = maybe (thisShouldNotHappen x) pure $
  getPrim x >>= (\(_, p) -> getter p)

getPrimBoolOrFail :: Lang -> Exec Bool
getPrimBoolOrFail = getPrimOrFailBy getPrimBool

getPrimTextOrFail :: Lang -> Exec Text
getPrimTextOrFail = getPrimOrFailBy getPrimText

getPrimIntOrFail :: Lang -> Exec Int64
getPrimIntOrFail = getPrimOrFailBy getPrimInt

getPrimBytesOrFail :: Lang -> Exec ByteString
getPrimBytesOrFail = getPrimOrFailBy getPrimBytes



{- for debug
traceFun :: (Show a, Show b) => String -> (a -> b) -> a -> b
traceFun name f x =
  let res = f x
  in  trace (mconcat ["\n\nTRACE: " , name, "(", show x, ") = ", show res]) (f x)

-- | We verify that expression is evaluated to the sigma-value that is
-- supplied by the proposer and then verify the proof itself.
exec :: ExecCtx -> TxArg -> (Bool, Text)
exec ctx tx
  | txPreservesValue tx = case res of
        Right (SigmaResult sigmaWithBools) -> case eliminateSigmaBool sigmaWithBools of
          Right sigma -> maybe (False, "No proof submitted") (\proof -> (S.equalSigmaProof sigma proof && S.verifyProof proof, debug)) mProof
          Left bool   -> (bool, "")
        Right (ConstBool bool)  -> (bool, "")
        Left err    -> (False, err)
  | otherwise = (False, "Sum of inputs does not equal to sum of outputs")
  where
    (res, debug) = execToSigma ctx tx
    mProof = txArg'proof tx



-- | Executes expression to sigma-expression
execToSigma :: ExecCtx -> TxArg -> (Either Text BoolExprResult, Text)
execToSigma ctx tx@TxArg{..} = execExpr $ getInputExpr tx
  where
    execExpr (Expr x) =
      case runExec ctx txArg'args (env'height txArg'env) txArg'inputs txArg'outputs $ execLang x of
        Right (Fix (PrimE _ (PrimSigma b)), msg)  -> case eliminateSigmaBool b of
          Left bool                               -> (Right $ ConstBool bool, msg)
          Right sigma                             -> (Right $ SigmaResult sigma, msg)
        Right (Fix (PrimE _ (PrimBool b)), msg)   -> (Right $ ConstBool b, msg)
        Right _                                   -> (Left noSigmaExpr, noSigmaExpr)
        Left err                                  -> (Left (showt err), showt err)

    noSigmaExpr = "Error: Script does not evaluate to sigma expression"

getInputExpr :: TxArg -> Expr Bool
getInputExpr tx@TxArg{..}
  | V.null inputs = onEmptyInputs
  | otherwise     = V.foldl1' (&&*) inputs
  where
    inputs = V.zipWith substSelfIndex (V.fromList [0..]) $ fmap (either (const false) applyBase . fromScript . box'script) txArg'inputs

    onEmptyInputs
      | isStartEpoch tx = true
      | otherwise       = false


applyBase :: Expr a -> Expr a
applyBase (Expr a) = Expr $ importBase a

substSelfIndex :: Int -> Expr a -> Expr a
substSelfIndex selfId (Expr x) = Expr $ cata phi x
  where
    phi = \case
      GetEnv loc idx -> Fix $ GetEnv loc $ case idx of
        Self src -> Input src $ Fix $ PrimE src $ PrimInt $ fromIntegral selfId
        _        -> idx
      other  -> Fix other

-}


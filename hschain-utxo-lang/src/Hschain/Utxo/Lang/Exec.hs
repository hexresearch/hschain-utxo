module Hschain.Utxo.Lang.Exec(
    exec
  , execLang
  , runExec
  , Error(..)
) where

import Hex.Common.Text

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict

import Crypto.Hash

import Data.Boolean
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

import Hschain.Utxo.Lang.Build()
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Lib.Base

import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Vector as V

removeAscr :: Lang -> Lang
removeAscr = cata $ \case
  Ascr a _ -> a
  other    -> Fix other

data Error
  = ParseError Text
  | AppliedNonFunction Lang
  | PoorlyTypedApplication Lang
  | UnboundVariables [VarName]
  | MismatchedBranches Lang
  | NonBooleanCond Lang
  | ThisShouldNotHappen Lang
  | BadUnaryOperator Lang
  | BadBinaryOperator Lang
  | BadTypeAscription Lang
  | IllegalRecursion Lang
  | OutOfBound Lang
  | NoField Text
  deriving (Show)

data Ctx = Ctx
  { ctx'vars       :: !(Map VarName Lang)
  , ctx'proof      :: !Proof
  , ctx'userArgs   :: !Args
  , ctx'height     :: !Integer
  , ctx'inputs     :: !(Vector Box)
  , ctx'outputs    :: !(Vector Box)
  , ctx'debug      :: !Text
  }

newtype Exec a = Exec (StateT Ctx (Either Error) a)
  deriving (MonadState Ctx, Monad, Functor, Applicative)

getProof :: Exec Proof
getProof = fmap ctx'proof get

getHeight :: Exec Int
getHeight = fmap (fromInteger . ctx'height) get

getInputs :: Exec (Vector Box)
getInputs = fmap ctx'inputs get

getOutputs :: Exec (Vector Box)
getOutputs = fmap ctx'outputs get

insertVar :: VarName -> Lang -> Exec ()
insertVar varName expr =
  modify' $ \st -> st { ctx'vars = M.insert varName expr $ ctx'vars st }

getUserArgs :: Exec Args
getUserArgs = fmap  ctx'userArgs get

saveTrace :: Text -> Exec ()
saveTrace msg =
  modify' $ \st -> st { ctx'debug = T.unlines [ctx'debug st, msg] }

runExec :: Proof -> Args -> Integer -> Vector Box -> Vector Box -> Exec a -> Either Error (a, Text)
runExec proof args height inputs outputs (Exec st) =
  fmap (second ctx'debug) $ runStateT st emptyCtx
  where
    emptyCtx = Ctx M.empty proof args height inputs outputs mempty

exec :: TxArg -> (Bool, Text)
exec tx@TxArg{..} =
  first (txPreservesValue tx &&) $ (execExpr $ getInputExpr tx)
  where
    execExpr (Expr x) =
      case runExec txArg'proof txArg'args (env'height txArg'env) txArg'inputs txArg'outputs $ execLang x of
        Right (Fix (PrimE (PrimBool b)), msg) -> (b, msg)
        Left err                              -> (False, showt err)

execLang :: Lang -> Exec Lang
execLang = execLang' . importBase

execLang' :: Lang -> Exec Lang
execLang' (Fix x) = case x of
    Var name -> getVar name
    PrimE p  -> pure $ Fix $ PrimE p
    Tuple as -> Fix . Tuple <$> mapM rec as
    Ascr a t -> fmap (\x -> Fix $ Ascr x t) $ rec a
    -- operations
    UnOpE uo x -> fromUnOp uo x
    BinOpE bi a b -> fromBiOp bi a b
    App a b -> fromApp a b
    Lam varName a b -> fromLam varName a b
    LamList vars a -> fromLamList vars a
    Let varName a b -> fromLet varName a b
    LetArg varName args a b -> fromLetArg varName args a b
    LetRec varName a b c -> fromLetRec varName a b c
    -- logic
    If a b c -> fromIf a b c
    Pk a -> fromPk a
    -- environment
    GetEnv idx -> fromEnv idx
    BoxE box -> fromBoxExpr box
    VecE vec -> fromVec vec
    TextE txt -> fromText txt
    Trace str a -> fromTrace str a
  where
    rec = execLang'

    getVar :: VarName -> Exec Lang
    getVar name = do
      vars <- fmap ctx'vars get
      case M.lookup name vars of
        Just res -> return res
        Nothing  -> Exec $ lift $ Left $ UnboundVariables [name]

    fromUnOp uo x = do
      x' <- rec x
      case uo of
        Not         -> fromNot x'
        Neg         -> fromNeg x'
        TupleAt n   -> fromTupleAt n x'
      where
        fromNot expr = case expr of
          Fix (PrimE (PrimBool b)) -> prim $ PrimBool $ not b
          _                        -> toError $ ThisShouldNotHappen x

        fromNeg expr = case expr of
          Fix (PrimE a) -> case a of
            PrimInt n    -> prim $ PrimInt $ negate n
            PrimDouble d -> prim $ PrimDouble $ negate d
            PrimMoney m  -> prim $ PrimMoney $ negate m
            other        -> thisShouldNotHappen $ Fix $ PrimE other
          _             -> thisShouldNotHappen x

        fromTupleAt n expr = case expr of
          Fix (Tuple as) -> maybe (outOfBound x) return $ as V.!? n
          _              -> thisShouldNotHappen x

    fromBiOp bi x y = do
      a <- rec x
      b <- rec y
      case bi of
        And       -> fromAnd a b
        Or        -> fromOr a b
        Plus      -> fromPlus a b
        Minus     -> fromMinus a b
        Times     -> fromTimes a b
        Div       -> fromDiv a b
        Equals    -> fromEq a b
        NotEquals -> fromNotEq a b
        LessThan  -> fromLt a b
        GreaterThan -> fromGt a b
        LessThanEquals -> fromLte a b
        GreaterThanEquals -> fromGte a b

    fromAnd, fromOr :: Lang -> Lang -> Exec Lang


    fromAnd (Fix x) (Fix y) = case x of
      PrimE (PrimBool a) ->
        if a
          then case y of
                  PrimE (PrimBool b) -> return $ Fix y
                  _                  -> thisShouldNotHappen $ Fix y
          else prim $ PrimBool False
      _                 -> thisShouldNotHappen $ Fix x

    fromOr (Fix x) (Fix y) = case x of
      PrimE (PrimBool a) ->
        if a
          then prim $ PrimBool True
          else case y of
                  PrimE (PrimBool b) -> prim $ PrimBool b
                  _                  -> thisShouldNotHappen $ Fix y
      _                  -> thisShouldNotHappen $ Fix x

    fromPlus  = fromNumOp2 Plus  (NumOp2 (+) (+) (+))
    fromMinus = fromNumOp2 Minus (NumOp2 (\x y -> x - y) (\x y -> x - y) (\x y -> x - y))
    fromTimes = fromNumOp2 Times (NumOp2 (*) (*) (*))
    fromDiv   = fromNumOp2 Div   (NumOp2 (/) div (/))

    fromNumOp2 op NumOp2{..} (Fix x) (Fix y) = case (x, y) of
      (PrimE a, PrimE b) -> case (a, b) of
        (PrimInt m, PrimInt n) -> prim $ PrimInt $ numOp2'int m n
        (PrimDouble m, PrimDouble n) -> prim $ PrimDouble $ numOp2'double m n
        (PrimMoney m, PrimMoney n) -> prim $ PrimMoney $ numOp2'money m n
        _ -> err
      _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE op (Fix x) (Fix y)


    fromEq (Fix x) (Fix y) = case (x, y) of
        (PrimE a, PrimE b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim $ PrimBool $ a1 == a2
          (PrimInt a1, PrimInt a2)       -> prim $ PrimBool $ a1 == a2
          (PrimDouble a1, PrimDouble a2) -> prim $ PrimBool $ a1 == a2
          (PrimMoney a1, PrimMoney a2)   -> prim $ PrimBool $ a1 == a2
          (PrimString a1, PrimString a2) -> prim $ PrimBool $ a1 == a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE Equals (Fix x) (Fix y)

    fromNotEq (Fix x) (Fix y) = case (x, y) of
        (PrimE a, PrimE b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim $ PrimBool $ a1 /= a2
          (PrimInt a1, PrimInt a2)       -> prim $ PrimBool $ a1 /= a2
          (PrimDouble a1, PrimDouble a2) -> prim $ PrimBool $ a1 /= a2
          (PrimMoney a1, PrimMoney a2)   -> prim $ PrimBool $ a1 /= a2
          (PrimString a1, PrimString a2) -> prim $ PrimBool $ a1 /= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE NotEquals (Fix x) (Fix y)

    fromLt (Fix x) (Fix y) = case (x, y) of
        (PrimE a, PrimE b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim $ PrimBool $ a1 < a2
          (PrimInt a1, PrimInt a2)       -> prim $ PrimBool $ a1 < a2
          (PrimDouble a1, PrimDouble a2) -> prim $ PrimBool $ a1 < a2
          (PrimMoney a1, PrimMoney a2)   -> prim $ PrimBool $ a1 < a2
          (PrimString a1, PrimString a2) -> prim $ PrimBool $ a1 < a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE LessThan (Fix x) (Fix y)

    fromGt (Fix x) (Fix y) = case (x, y) of
        (PrimE a, PrimE b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim $ PrimBool $ a1 > a2
          (PrimInt a1, PrimInt a2)       -> prim $ PrimBool $ a1 > a2
          (PrimDouble a1, PrimDouble a2) -> prim $ PrimBool $ a1 > a2
          (PrimMoney a1, PrimMoney a2)   -> prim $ PrimBool $ a1 > a2
          (PrimString a1, PrimString a2) -> prim $ PrimBool $ a1 > a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE GreaterThan (Fix x) (Fix y)

    fromLte (Fix x) (Fix y) = case (x, y) of
        (PrimE a, PrimE b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim $ PrimBool $ a1 <= a2
          (PrimInt a1, PrimInt a2)       -> prim $ PrimBool $ a1 <= a2
          (PrimDouble a1, PrimDouble a2) -> prim $ PrimBool $ a1 <= a2
          (PrimMoney a1, PrimMoney a2)   -> prim $ PrimBool $ a1 <= a2
          (PrimString a1, PrimString a2) -> prim $ PrimBool $ a1 <= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE LessThanEquals (Fix x) (Fix y)

    fromGte (Fix x) (Fix y) = case (x, y) of
        (PrimE a, PrimE b) -> case (a, b) of
          (PrimBool a1, PrimBool a2)     -> prim $ PrimBool $ a1 >= a2
          (PrimInt a1, PrimInt a2)       -> prim $ PrimBool $ a1 >= a2
          (PrimDouble a1, PrimDouble a2) -> prim $ PrimBool $ a1 >= a2
          (PrimMoney a1, PrimMoney a2)   -> prim $ PrimBool $ a1 >= a2
          (PrimString a1, PrimString a2) -> prim $ PrimBool $ a1 >= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE GreaterThanEquals (Fix x) (Fix y)

    fromIf cond t e  = do
      Fix cond' <- rec cond
      case cond' of
        PrimE (PrimBool b) -> if b then rec t else rec e
        _                  -> err
      where
        err = thisShouldNotHappen $ Fix $ If cond t e

    fromLam name a b = return $ Fix $ Lam name a b
    fromLamList vars a = rec $ unfoldLamList vars a

    unfoldLamList vars a = L.foldl' (\z a -> z . Fix . uncurry Lam a) id vars a

    fromTrace str a = do
      Fix str' <- rec str
      case str' of
        PrimE (PrimString msg) -> do
          saveTrace msg
          rec a
        _ -> thisShouldNotHappen $ Fix str'

    fromPk x = do
      Fix x' <- rec x
      case x' of
        PrimE (PrimString pkey) -> prim . PrimBool =<< fmap (checkPubKey pkey) getProof
        _                       -> thisShouldNotHappen x

    fromApp fun arg = case fun of
      Fix (App (Fix (VecE VecMap)) f) -> do
        Fix f' <- rec f
        Fix vec' <- rec arg
        let errVal = fun
        case vec' of
          VecE (NewVec vs) -> rec $ Fix $ VecE (NewVec $ fmap (Fix . App (Fix f')) vs)
          _ -> thisShouldNotHappen errVal
      Fix (App (Fix (App (Fix (VecE VecFold)) f)) z) -> do
        fun' <- rec f
        z'   <- rec z
        Fix vec' <- rec arg
        let errVal = fun
        case vec' of
          VecE (NewVec vs) -> rec $ V.foldl (\a b -> app2 fun' a b) z' vs
          _ -> thisShouldNotHappen errVal
      Fix (VecE VecLength) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim . PrimInt) $ vecSize arg'
      Fix (TextE TextLength) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim . PrimInt) $ textSize arg'
      Fix (TextE ConvertToText) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim . PrimString) $ convertToText arg'
      Fix (TextE (TextHash Sha256)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim . PrimString) $ sha256 arg'
      Fix (TextE (TextHash Blake2b256)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim . PrimString) $ blake2b256 arg'
      _ -> do
        Fix fun' <- rec fun
        case fun' of
          Lam varName _ body -> do
            arg' <- rec arg
            rec $ subst body varName arg'
          VecE VecMap -> do
            arg' <- rec arg
            return $ Fix $ App (Fix (VecE VecMap)) arg'
          VecE VecFold -> do
            arg' <- rec arg
            return $ Fix $ App (Fix (VecE VecFold)) arg'
          App (Fix (VecE VecFold)) a -> do
            a' <- rec a
            arg' <- rec arg
            return $ Fix $ App (Fix $ App (Fix (VecE VecFold)) a') arg'
          other              -> Exec $ lift $ Left $ AppliedNonFunction $ Fix other

    fromLet v lc1 lc2 = do
      lc1' <- rec lc1
      rec $ subst lc2 v lc1'

    fromLetArg v args a b = rec $ unfoldLetArg v args a b

    unfoldLetArg v args a b = Fix $ Let v (Fix $ LamList (fmap (, Fix UknownType) args) a) b

    fromLetRec v ty lc1 lc2 = do
      insertVar v lc1
      lc1' <- rec lc1
      case lc1' of
        lam@(Fix (Lam _ _ _)) -> do
          insertVar v lc1
          rec $ subst lc2 v lam
        _ -> Exec $ lift $ Left $ IllegalRecursion $ Fix $ LetRec v ty lc1 lc2

    fromEnv idx = do
      idx' <- mapM rec idx
      case idx' of
        Height   -> prim . PrimInt =<< getHeight
        Input  (Fix (PrimE (PrimInt n))) -> toBox n =<< getInputs
        Output (Fix (PrimE (PrimInt n))) -> toBox n =<< getOutputs
        Inputs  -> fmap toBoxes getInputs
        Outputs -> fmap toBoxes getOutputs
        GetVar (Fix (PrimE (PrimString argName))) -> do
          args <- getUserArgs
          case M.lookup argName args of
            Just value -> prim value
            _          -> noField argName
        _      -> return $ Fix $ GetEnv idx
      where
        toBox n v = maybe (outOfBound $ Fix $ GetEnv idx) (pure . Fix . BoxE . PrimBox) $ v V.!? n
        toBoxes vs = Fix $ VecE $ NewVec $ fmap (Fix . BoxE . PrimBox) vs

    fromBoxField (Fix x) field = case x of
      GetEnv (Input n)  -> getBoxField field
      GetEnv (Output n) -> getBoxField field

    fromBoxExpr :: BoxExpr Lang -> Exec Lang
    fromBoxExpr x = do
      x' <- mapM rec x
      case x' of
        PrimBox box -> return $ Fix $ BoxE $ PrimBox box
        BoxAt (Fix (BoxE (PrimBox box))) field -> getBoxField box field
        _ -> thisShouldNotHappen $ Fix $ BoxE x


    getBoxField Box{..} field = case field of
      BoxFieldId      -> prim $ PrimString $ unBoxId box'id
      BoxFieldValue   -> prim $ PrimMoney  $ box'value
      BoxFieldScript  -> prim $ PrimString $ unScript $ box'script
      BoxFieldArg txt -> case txt of
        Fix (PrimE (PrimString t)) -> maybe (noField t) prim $ M.lookup t box'args
        _                          -> thisShouldNotHappen txt

    fromVec x = do
      x' <- mapM rec x
      case x' of
        NewVec v        -> fmap (Fix . VecE . NewVec) $ mapM rec v
        VecAppend a b   -> do
          a' <- rec a
          b' <- rec b
          return $ Fix $ VecE $ case (a', b') of
            (Fix (VecE (NewVec v1)), Fix (VecE (NewVec v2))) -> NewVec $ mappend v1 v2
            _ -> VecAppend a' b'
        VecAt v n       -> do
          Fix v' <- rec v
          Fix n' <- rec n
          let errVal = Fix $ VecE $ VecAt (Fix v') (Fix n')
          res <- case (v', n') of
            (VecE (NewVec vs), PrimE (PrimInt idx)) -> maybe (outOfBound errVal) return $ vs V.!? idx
            _ -> thisShouldNotHappen errVal
          rec res
        VecMap -> return $ Fix $ VecE VecMap
        VecFold -> return $ Fix $ VecE VecFold
        VecLength -> return $ Fix $ VecE VecLength

    fromText x = do
      x' <- mapM rec x
      case x' of
        TextAppend a b -> do
          a' <- rec a
          b' <- rec b
          return $ Fix $ case (a', b') of
            (Fix (PrimE(PrimString t1)), Fix (PrimE (PrimString t2))) -> PrimE $ PrimString $ mappend t1 t2
            _                                                         -> TextE $ TextAppend a' b'
        ConvertToText -> returnText ConvertToText
        TextLength    -> returnText TextLength
        TextHash algo   -> returnText $ TextHash algo
        where
          returnText = return . Fix . TextE

    textSize (Fix x) = case x of
      PrimE (PrimString txt) -> Just $ T.length txt
      _                      -> Nothing

    vecSize (Fix x) = case x of
      VecE (NewVec xs)      -> Just $ V.length xs
      VecE (VecAppend a b)  -> liftA2 (+) (vecSize a) (vecSize b)
      _                     -> Nothing

    convertToText (Fix x) = case x of
      PrimE p  -> Just $ convertPrim p
      _        -> Nothing
      where
        convertPrim = \case
          PrimInt n       -> showt n
          PrimMoney m     -> showt m
          PrimDouble d    -> showt d
          PrimString t    -> t
          PrimBool b      -> showt b

    sha256 (Fix x) = case x of
      PrimE (PrimString t) -> Just $ hashText t
      _                    -> Nothing
      where
        hashText = showt . hashWith SHA256 . T.encodeUtf8

    blake2b256 (Fix x) = case x of
      PrimE (PrimString t) -> Just $ hashText t
      _                    -> Nothing
      where
        hashText = showt . hashWith Blake2b_256 . T.encodeUtf8

    subst :: Lang -> VarName -> Lang -> Lang
    subst (Fix body) varName sub = case body of
      Var e                | e == varName  -> sub
                           | otherwise     -> Fix $ Var e
      PrimE p                              -> Fix body
      Ascr lc t                            -> Fix $ Ascr (rec lc) t
      UnOpE uo lc                          -> Fix $ UnOpE uo $ rec lc
      BinOpE bo a b                        -> Fix $ BinOpE bo (rec a) (rec b)
      App a b                              -> Fix $ App (rec a) (rec b)
      e@(Lam v1 ty body1)  | v1 == varName -> Fix $ e
                           | otherwise     -> Fix $ Lam v1 ty (rec body1)
      If cond t e                          -> Fix $ If (rec cond) (rec t) (rec e)
      Let v1 a1 a2         | v1 == varName -> Fix $ Let v1 a1 (rec a2)
                           | otherwise     -> Fix $ Let v1 (rec a1) (rec a2)
      LetRec v1 ty a1 a2   | v1 == varName -> Fix $ LetRec v1 ty a1 (rec a2)
                           | otherwise     -> Fix $ LetRec v1 ty (rec a1) (rec a2)
      Pk a                                 -> Fix $ Pk $ rec a
      Tuple as                             -> Fix $ Tuple $ fmap rec as
      GetEnv idx                           -> Fix $ GetEnv $ fmap rec idx
      VecE vec                             -> Fix $ VecE $ fmap rec vec
      TextE txt                            -> Fix $ TextE $ fmap rec txt
      BoxE box                             -> Fix $ BoxE $ fmap rec box
      LamList vs a                         -> rec $ unfoldLamList vs a
      LetArg name args a b                 -> rec $ unfoldLetArg name args a b
      Trace a b                            -> Fix $ Trace (rec a) (rec b)
      where
        rec x = subst x varName sub

prim :: Prim -> Exec Lang
prim p = return $ Fix $ PrimE p

toError :: Error -> Exec a
toError = Exec . lift . Left

thisShouldNotHappen :: Lang -> Exec Lang
thisShouldNotHappen = toError . ThisShouldNotHappen

noField :: Text -> Exec Lang
noField = toError . NoField

outOfBound :: Lang -> Exec Lang
outOfBound = toError . OutOfBound

type Mono2 a = a -> a -> a

data NumOp2 = NumOp2
  { numOp2'double :: Mono2 Double
  , numOp2'int    :: Mono2 Int
  , numOp2'money  :: Mono2 Money
  }

getInputExpr :: TxArg -> Expr Bool
getInputExpr tx@TxArg{..}
  | V.null inputs = onEmptyInputs
  | otherwise     = V.foldl1' (&&*) inputs
  where
    inputs = V.zipWith substSelfIndex (V.fromList [0..]) $ fmap (fromMaybe false . fromScript . box'script) txArg'inputs

    onEmptyInputs
      | isStartEpoch tx = true
      | otherwise       = false

substSelfIndex :: Int -> Expr a -> Expr a
substSelfIndex selfId (Expr x) = Expr $ cata phi x
  where
    phi = \case
      GetEnv idx -> Fix $ GetEnv $ case idx of
        Self -> Input $ Fix $ PrimE $ PrimInt selfId
        _    -> idx
      other  -> Fix other

isStartEpoch :: TxArg -> Bool
isStartEpoch TxArg{..} = env'height txArg'env == 0

txPreservesValue :: TxArg -> Bool
txPreservesValue tx@TxArg{..}
  | isStartEpoch tx = True
  | otherwise       = toSum txArg'inputs == toSum txArg'outputs
  where
    toSum xs = getSum $ foldMap (Sum . box'value) xs

app2 :: Lang -> Lang -> Fix E -> Lang
app2 f a b = Fix (App (Fix (App f a)) b)


{-
traceFun :: (Show a, Show b) => String -> (a -> b) -> a -> b
traceFun name f x =
  let res = f x
  in  trace (mconcat ["\n\nTRACE: " , name, "(", show x, ") = ", show res]) (f x)
-}

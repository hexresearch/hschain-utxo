module Hschain.Utxo.Lang.Exec(
    exec
  , execLang
  , execToSigma
  , runExec
  , Error(..)
) where

import Hex.Common.Control
import Hex.Common.Text

import Control.Applicative
import Control.Arrow
import Control.Monad.State.Strict

import Crypto.Hash

import Data.Boolean
import Data.Fix
import Data.Map.Strict (Map)
import Data.Maybe
import Data.Monoid hiding (Alt)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)

import Type.Loc
import Type.Type

import Hschain.Utxo.Lang.Build()
import Hschain.Utxo.Lang.Desugar
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
  Ascr _ a _ -> a
  other      -> Fix other

data Error
  = ParseError Loc Text
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
  | NoField VarName
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
        Right (Fix (PrimE _ (PrimBool _ b)), msg) -> (b, msg)
        Left err                                  -> (False, showt err)

execLang :: Lang -> Exec Lang
execLang = execLang' . importBase

execLang' :: Lang -> Exec Lang
execLang' (Fix x) = case x of
    Var loc name -> getVar loc name
    PrimE loc p  -> pure $ Fix $ PrimE loc p
    Tuple loc as -> Fix . Tuple loc <$> mapM rec as
    Ascr loc a t -> fmap (\x -> Fix $ Ascr loc x t) $ rec a
    -- operations
    UnOpE loc uo x -> fromUnOp loc uo x
    BinOpE loc bi a b -> fromBiOp loc bi a b
    Apply loc a b -> fromApply loc a b
    InfixApply loc a v b -> fromInfixApply loc a v b
    Lam loc varName b -> fromLam loc varName b
    LamList loc vars a -> fromLamList loc vars a
    Let loc varName a -> fromLet loc varName a
    LetRec loc varName b c -> fromLetRec loc varName b c
    -- logic
    If loc a b c -> fromIf loc a b c
    Pk loc a -> fromPk loc a
    -- environment
    GetEnv loc idx -> fromEnv loc idx
    BoxE loc box -> fromBoxExpr loc box
    VecE loc vec -> fromVec loc vec
    TextE loc txt -> fromText loc txt
    Trace loc str a -> fromTrace loc str a
  where
    rec = execLang'

    getVar :: Loc -> VarName -> Exec Lang
    getVar loc name = do
      vars <- fmap ctx'vars get
      case M.lookup name vars of
        Just res -> return res
        Nothing  -> Exec $ lift $ Left $ UnboundVariables [name]

    fromUnOp loc uo x = do
      x' <- rec x
      case uo of
        Not         -> fromNot x'
        Neg         -> fromNeg x'
        TupleAt n   -> fromTupleAt n x'
      where
        fromNot expr = case expr of
          Fix (PrimE loc1 (PrimBool loc2 b))  -> prim loc1 $ PrimBool loc2 $ not b
          Fix (PrimE loc1 (PrimSigma loc2 b)) -> prim loc1 $ either (PrimBool loc2) (PrimSigma loc2) $ notSigma b
          _                                   -> thisShouldNotHappen x

        fromNeg expr = case expr of
          Fix (PrimE loc1 a) -> case a of
            PrimInt loc2 n    -> prim loc1 $ PrimInt loc2 $ negate n
            PrimDouble loc2 d -> prim loc1 $ PrimDouble loc2 $ negate d
            PrimMoney loc2 m  -> prim loc1 $ PrimMoney loc2 $ negate m
            other             -> thisShouldNotHappen $ Fix $ PrimE loc1 other
          _             -> thisShouldNotHappen x

        fromTupleAt n expr = case expr of
          Fix (Tuple loc1 as) -> maybe (outOfBound x) return $ as V.!? n
          _                   -> thisShouldNotHappen x

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
        ComposeFun -> fromComposeFun loc a b

    fromAnd, fromOr :: Loc -> Lang -> Lang -> Exec Lang

    -- todo: maybe it's worth to make it lazy
    fromAnd loc x y = do
      Fix x' <- rec x
      Fix y' <- rec y
      case (x', y') of
        (PrimE locX1 (PrimBool locX2 a), _) ->
          if a
            then return (Fix y')
            else prim locX1 $ PrimBool locX2 False
        (_, PrimE locX1 (PrimBool locX2 a)) ->
          if a
            then return (Fix x')
            else prim locX1 $ PrimBool locX2 False
        (PrimE locX1 (PrimSigma _ a), PrimE locY1 (PrimSigma _ b)) ->
          return $ Fix $ PrimE loc $ PrimSigma loc $ Fix $ SigmaAnd () a b
        _                 -> thisShouldNotHappen $ Fix $ BinOpE loc And x y

    -- todo: maybe it's worth to make it lazy
    fromOr loc x y = do
      Fix x' <- rec x
      Fix y' <- rec y
      case (x', y') of
        (PrimE locX1 (PrimBool locX2 a), _) ->
          if a
            then prim locX1 $ PrimBool locX2 True
            else return (Fix y')
        (_, PrimE locX1 (PrimBool locX2 a)) ->
          if a
            then prim locX1 $ PrimBool locX2 True
            else return (Fix x')
        (PrimE locX1 (PrimSigma _ a), PrimE locY1 (PrimSigma _ b)) ->
          return $ Fix $ PrimE loc $ PrimSigma loc $ Fix $ SigmaOr () a b
        _                 -> thisShouldNotHappen $ Fix $ BinOpE loc And x y

    fromPlus  loc = fromNumOp2 loc Plus  (NumOp2 (+) (+) (+))
    fromMinus loc = fromNumOp2 loc Minus (NumOp2 (\x y -> x - y) (\x y -> x - y) (\x y -> x - y))
    fromTimes loc = fromNumOp2 loc Times (NumOp2 (*) (*) (*))
    fromDiv   loc = fromNumOp2 loc Div   (NumOp2 (/) div (/))

    fromNumOp2 loc op NumOp2{..} (Fix x) (Fix y) = case (x, y) of
      (PrimE locA1 a, PrimE locB1 b) -> case (a, b) of
        (PrimInt locA2 m, PrimInt _ n) -> prim locA1 $ PrimInt locA2 $ numOp2'int m n
        (PrimDouble locA2 m, PrimDouble _ n) -> prim locA1 $ PrimDouble locA2 $ numOp2'double m n
        (PrimMoney locA2 m, PrimMoney _ n) -> prim locA1 $ PrimMoney locA2 $ numOp2'money m n
        _ -> err
      _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc op (Fix x) (Fix y)


    fromEq loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool locA2 a1, PrimBool _ a2)     -> prim locA1 $ PrimBool locA2 $ a1 == a2
          (PrimInt locA2 a1, PrimInt _ a2)       -> prim locA1 $ PrimBool locA2 $ a1 == a2
          (PrimDouble locA2 a1, PrimDouble _ a2) -> prim locA1 $ PrimBool locA2 $ a1 == a2
          (PrimMoney locA2 a1, PrimMoney _ a2)   -> prim locA1 $ PrimBool locA2 $ a1 == a2
          (PrimString locA2 a1, PrimString _ a2) -> prim locA1 $ PrimBool locA2 $ a1 == a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc Equals (Fix x) (Fix y)

    fromNotEq loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool locA2 a1, PrimBool _ a2)     -> prim locA1 $ PrimBool locA2 $ a1 /= a2
          (PrimInt locA2 a1, PrimInt _ a2)       -> prim locA1 $ PrimBool locA2 $ a1 /= a2
          (PrimDouble locA2 a1, PrimDouble _ a2) -> prim locA1 $ PrimBool locA2 $ a1 /= a2
          (PrimMoney locA2 a1, PrimMoney _ a2)   -> prim locA1 $ PrimBool locA2 $ a1 /= a2
          (PrimString locA2 a1, PrimString _ a2) -> prim locA1 $ PrimBool locA2 $ a1 /= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc NotEquals (Fix x) (Fix y)

    fromLt loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool locaA2 a1, PrimBool _ a2)     -> prim locA1 $ PrimBool locaA2 $ a1 < a2
          (PrimInt locaA2 a1, PrimInt _ a2)       -> prim locA1 $ PrimBool locaA2 $ a1 < a2
          (PrimDouble locaA2 a1, PrimDouble _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 < a2
          (PrimMoney locaA2 a1, PrimMoney _ a2)   -> prim locA1 $ PrimBool locaA2 $ a1 < a2
          (PrimString locaA2 a1, PrimString _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 < a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc LessThan (Fix x) (Fix y)

    fromGt loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool locaA2 a1, PrimBool _ a2)     -> prim locA1 $ PrimBool locaA2 $ a1 > a2
          (PrimInt locaA2 a1, PrimInt _ a2)       -> prim locA1 $ PrimBool locaA2 $ a1 > a2
          (PrimDouble locaA2 a1, PrimDouble _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 > a2
          (PrimMoney locaA2 a1, PrimMoney _ a2)   -> prim locA1 $ PrimBool locaA2 $ a1 > a2
          (PrimString locaA2 a1, PrimString _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 > a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc GreaterThan (Fix x) (Fix y)

    fromLte loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool locaA2 a1, PrimBool _ a2)     -> prim locA1 $ PrimBool locaA2 $ a1 <= a2
          (PrimInt locaA2 a1, PrimInt _ a2)       -> prim locA1 $ PrimBool locaA2 $ a1 <= a2
          (PrimDouble locaA2 a1, PrimDouble _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 <= a2
          (PrimMoney locaA2 a1, PrimMoney _ a2)   -> prim locA1 $ PrimBool locaA2 $ a1 <= a2
          (PrimString locaA2 a1, PrimString _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 <= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc LessThanEquals (Fix x) (Fix y)

    fromGte loc (Fix x) (Fix y) = case (x, y) of
        (PrimE locA1 a, PrimE _ b) -> case (a, b) of
          (PrimBool locaA2 a1, PrimBool _ a2)     -> prim locA1 $ PrimBool locaA2 $ a1 >= a2
          (PrimInt locaA2 a1, PrimInt _ a2)       -> prim locA1 $ PrimBool locaA2 $ a1 >= a2
          (PrimDouble locaA2 a1, PrimDouble _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 >= a2
          (PrimMoney locaA2 a1, PrimMoney _ a2)   -> prim locA1 $ PrimBool locaA2 $ a1 >= a2
          (PrimString locaA2 a1, PrimString _ a2) -> prim locA1 $ PrimBool locaA2 $ a1 >= a2
          _ -> err
        _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc GreaterThanEquals (Fix x) (Fix y)

    fromComposeFun loc (Fix x) (Fix y) = case (x, y) of
      (Lam loc1 v2 f2, Lam loc2 v1 f1) -> rec $ Fix $ Lam loc v1 (Fix $ Apply loc1 (Fix $ Lam loc2 v2 f2) f1)
      _ -> err
      where
        err = thisShouldNotHappen $ Fix $ BinOpE loc ComposeFun (Fix x) (Fix y)

    fromIf loc cond t e  = do
      Fix cond' <- rec cond
      case cond' of
        PrimE _ (PrimBool _ b) -> if b then rec t else rec e
        _                      -> err
      where
        err = thisShouldNotHappen $ Fix $ If loc cond t e

    fromLam loc name b = return $ Fix $ Lam loc name b
    fromLamList loc vars a = rec $ unfoldLamList loc vars a

    fromTrace _ str a = do
      Fix str' <- rec str
      case str' of
        PrimE _ (PrimString _ msg) -> do
          saveTrace msg
          rec a
        _ -> thisShouldNotHappen $ Fix str'

    fromPk loc x = do
      Fix x' <- rec x
      case x' of
        PrimE loc1 (PrimString loc2 pkey) -> return $ Fix $ PrimE loc $ PrimSigma loc1 $ Fix $ SigmaPk () pkey --  prim loc1 . PrimBool loc2 =<< fmap (checkPubKey pkey) getProof
        _                                 -> thisShouldNotHappen x

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
        maybe (thisShouldNotHappen arg') (prim loc . PrimInt loc) $ vecSize arg'
      Fix (TextE _ (TextLength _)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim loc . PrimInt loc) $ textSize arg'
      Fix (TextE _ (ConvertToText _)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim loc . PrimString loc) $ convertToText arg'
      Fix (TextE _ (TextHash _ Sha256)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim loc . PrimString loc) $ sha256 arg'
      Fix (TextE _ (TextHash _ Blake2b256)) -> do
        arg' <- rec arg
        maybe (thisShouldNotHappen arg') (prim loc . PrimString loc) $ blake2b256 arg'
      _ -> do
        Fix fun' <- rec fun
        case fun' of
          Lam _ varName body -> do
            arg' <- rec arg
            rec $ subst body varName arg'
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
          other              -> Exec $ lift $ Left $ AppliedNonFunction $ Fix other



    fromLet loc bg expr = execDefs defs expr
      where
        defs = concat
          [ fmap explToImpl (bindGroup'expl bg)
          , concat $ bindGroup'impl bg ]

        execDefs ds e = case ds of
          [] -> rec e
          def:rest -> do
            let v = toVarName $ impl'name def
            body <- execAlts $ impl'alts def
            execDefs (fmap2 (\x -> subst x v body) rest) (subst e v body)

        execAlts :: [Alt Lang] -> Exec Lang
        execAlts as = case as of
          a:_ -> execAlt a
          _   -> thisShouldNotHappen $ Fix $ Let loc bg expr

        execAlt :: Alt Lang -> Exec Lang
        execAlt Alt{..} = return $ case alt'pats of
          [] -> alt'expr
          ps -> Fix $ LamList loc (fmap toVars ps) $ alt'expr
          where
            toVars (PVar _ var) = toVarName var

    fromLetRec loc v lc1 lc2 = do
      insertVar v lc1
      lc1' <- rec lc1
      case lc1' of
        lam@(Fix (Lam _ _ _)) -> do
          insertVar v lc1
          rec $ subst lc2 v lam
        _ -> Exec $ lift $ Left $ IllegalRecursion $ Fix $ LetRec loc v lc1 lc2

    fromEnv loc idx = do
      idx' <- mapM rec idx
      case idx' of
        Height loc1  -> prim loc . PrimInt loc1 =<< getHeight
        Input loc1 (Fix (PrimE _ (PrimInt _ n))) -> toBox loc1 n =<< getInputs
        Output loc1 (Fix (PrimE _ (PrimInt _ n))) -> toBox loc1 n =<< getOutputs
        Inputs loc1  -> fmap (toBoxes loc1) getInputs
        Outputs loc1 -> fmap (toBoxes loc1) getOutputs
        GetVar _ (Fix (PrimE _ (PrimString _ argName))) -> do
          args <- getUserArgs
          case M.lookup argName args of
            Just value -> prim loc value
            _          -> noField (VarName loc argName)
        _      -> return $ Fix $ GetEnv loc idx
      where
        toBox loc1 n v = maybe (outOfBound $ Fix $ GetEnv loc idx) (pure . Fix . BoxE loc1 . PrimBox loc1) $ v V.!? n
        toBoxes loc1 vs = Fix $ VecE loc $ NewVec loc $ fmap (Fix . BoxE loc1 . PrimBox loc1) vs

    fromBoxExpr :: Loc -> BoxExpr Lang -> Exec Lang
    fromBoxExpr loc x = do
      x' <- mapM rec x
      case x' of
        PrimBox loc1 box -> return $ Fix $ BoxE loc $ PrimBox loc1 box
        BoxAt loc1 (Fix (BoxE _ (PrimBox _ box))) field -> getBoxField loc1 box field
        _ -> thisShouldNotHappen $ Fix $ BoxE loc x

    getBoxField :: Loc -> Box -> BoxField Lang -> Exec Lang
    getBoxField loc Box{..} field = case field of
      BoxFieldId      -> prim loc $ PrimString loc $ unBoxId box'id
      BoxFieldValue   -> prim loc $ PrimMoney  loc $ box'value
      BoxFieldScript  -> prim loc $ PrimString loc $ unScript $ box'script
      BoxFieldArg txt -> case txt of
        Fix (PrimE loc1 (PrimString loc2 t)) -> maybe (noField $ VarName loc2 t) (prim loc1) $ M.lookup t box'args
        _                                    -> thisShouldNotHappen txt

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
            (VecE _ (NewVec _ vs), PrimE _ (PrimInt _ idx)) -> maybe (outOfBound errVal) return $ vs V.!? idx
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
            (Fix (PrimE _ (PrimString _ t1)), Fix (PrimE _ (PrimString _ t2))) -> PrimE loc $ PrimString loc $ mappend t1 t2
            _                                                                  -> TextE loc $ TextAppend loc a' b'
        ConvertToText loc1  -> returnText $ ConvertToText loc1
        TextLength loc1     -> returnText $ TextLength loc1
        TextHash loc1 algo  -> returnText $ TextHash loc1 algo
        where
          returnText = return . Fix . TextE loc

    textSize (Fix x) = case x of
      PrimE _ (PrimString _ txt) -> Just $ T.length txt
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
          PrimInt _ n       -> showt n
          PrimMoney _ m     -> showt m
          PrimDouble _ d    -> showt d
          PrimString _ t    -> t
          PrimBool _ b      -> showt b

    sha256 (Fix x) = case x of
      PrimE _ (PrimString _ t) -> Just $ hashText t
      _                        -> Nothing
      where
        hashText = showt . hashWith SHA256 . T.encodeUtf8

    blake2b256 (Fix x) = case x of
      PrimE _ (PrimString _ t) -> Just $ hashText t
      _                        -> Nothing
      where
        hashText = showt . hashWith Blake2b_256 . T.encodeUtf8

    subst :: Lang -> VarName -> Lang -> Lang
    subst (Fix body) varName sub = case body of
      Var loc e                | e == varName  -> sub
                               | otherwise     -> Fix $ Var loc e
      PrimE _ p                                -> Fix body
      Ascr loc lc t                            -> Fix $ Ascr loc (rec lc) t
      UnOpE loc uo lc                          -> Fix $ UnOpE loc uo $ rec lc
      BinOpE loc bo a b                        -> Fix $ BinOpE loc bo (rec a) (rec b)
      Apply loc a b                            -> Fix $ Apply loc (rec a) (rec b)
      InfixApply loc a v b     | v == varName  -> subInfix loc sub a b
      InfixApply loc a v b     | otherwise     -> Fix $ InfixApply loc (rec a) v (rec b)
      e@(Lam loc v1 body1)     | v1 == varName -> Fix $ e
                               | otherwise     -> Fix $ Lam loc v1 (rec body1)
      If loc cond t e                          -> Fix $ If loc (rec cond) (rec t) (rec e)
      Let loc bg e                             -> Fix $ Let loc (substBindGroup bg) (rec e)
                        -- | v1 == varName -> Fix $ Let v1 a1 (rec a2)
                        -- | otherwise     -> Fix $ Let v1 (rec a1) (rec a2)
      LetRec loc v1 a1 a2      | v1 == varName -> Fix $ LetRec loc v1 a1 (rec a2)
                               | otherwise     -> Fix $ LetRec loc v1 (rec a1) (rec a2)
      Pk loc a                                 -> Fix $ Pk loc $ rec a
      Tuple loc as                             -> Fix $ Tuple loc $ fmap rec as
      GetEnv loc idx                           -> Fix $ GetEnv loc $ fmap rec idx
      VecE loc vec                             -> Fix $ VecE loc $ fmap rec vec
      TextE loc txt                            -> Fix $ TextE loc $ fmap rec txt
      BoxE loc box                             -> Fix $ BoxE loc $ fmap rec box
      LamList loc vs a                         -> rec $ unfoldLamList loc vs a
      Trace loc a b                            -> Fix $ Trace loc (rec a) (rec b)
      where
        subInfix loc op a b = rec $ Fix (Apply loc (Fix $ Apply loc op a) b)

        rec x = subst x varName sub

        substBindGroup BindGroup{..} = BindGroup
          { bindGroup'expl = fmap  substExpl bindGroup'expl
          , bindGroup'impl = fmap2 substImpl bindGroup'impl
          }

        substExpl x@Expl{..}
          | varName == toVarName expl'name = x
          | otherwise                      = x { expl'alts = fmap substAlt expl'alts }

        substImpl x@Impl{..}
          | varName == toVarName impl'name = x
          | otherwise                      = x { impl'alts = fmap substAlt impl'alts }

        substAlt x@Alt{..}
          | isBinded varName alt'pats = x
          | otherwise                 = x{ alt'expr = rec alt'expr }
          where
            isBinded v ps = fromVarName v `elem` (foldMap getBinds ps)

            getBinds = \case
              PVar _ idx -> [idx]
          --     _          -> []

prim :: Loc -> Prim -> Exec Lang
prim loc p = return $ Fix $ PrimE loc p

toError :: Error -> Exec a
toError = Exec . lift . Left

thisShouldNotHappen :: Lang -> Exec Lang
thisShouldNotHappen = toError . ThisShouldNotHappen

noField :: VarName -> Exec Lang
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
      GetEnv loc idx -> Fix $ GetEnv loc $ case idx of
        Self loc -> Input loc $ Fix $ PrimE loc $ PrimInt loc selfId
        _        -> idx
      other  -> Fix other

isStartEpoch :: TxArg -> Bool
isStartEpoch TxArg{..} = env'height txArg'env == 0

txPreservesValue :: TxArg -> Bool
txPreservesValue tx@TxArg{..}
  | isStartEpoch tx = True
  | otherwise       = toSum txArg'inputs == toSum txArg'outputs
  where
    toSum xs = getSum $ foldMap (Sum . box'value) xs


{-
traceFun :: (Show a, Show b) => String -> (a -> b) -> a -> b
traceFun name f x =
  let res = f x
  in  trace (mconcat ["\n\nTRACE: " , name, "(", show x, ") = ", show res]) (f x)
-}

execToSigma :: TxArg -> (Either Text Sigma', Text)
execToSigma tx@TxArg{..} = execExpr $ getInputExpr tx
  where
    execExpr (Expr x) =
      case runExec txArg'proof txArg'args (env'height txArg'env) txArg'inputs txArg'outputs $ execLang x of
        Right (Fix (PrimE _ (PrimSigma _ b)), msg) -> (Right b, msg)
        Right _                                    -> (Left noSigmaExpr, noSigmaExpr)
        Left err                                   -> (Left (showt err), showt err)

    noSigmaExpr = "Error: Script does not evaluate to sigma expression"



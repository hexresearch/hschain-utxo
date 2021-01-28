{-# LANGUAGE CPP #-}
-- | QuasiQuoter for high level language.
--
-- With quasi quoter we can create scripts quote easily. We use prefix utxo:
--
-- > box'script = [utxo|main = (2 + 2) == 4|]
--
--  QuasiQuote is trasformed to value of type Script.
--  It accepts two types of code. First type is code for expressions.
--  It is treated as if it's module with single main function defined with given expression.
--
-- > script = [utxo|lengthText "Hello"|]
--
-- It would be converted to script with single main function:
--
-- > main = lengthText "Hello"
--
--  Also it accepts full modules with set of definitions.
--
--  Dependencies: for quasi quoter to work we should import module @Hschain.Utxo.Lang@
--  and enable extension @OverloadedStrings@.
--
-- For interpolation of external haskell values we use dollar sign with single parens around the variable:
--
-- > [utxo|main = pk $(alicePubKey)|]
--
-- Value is inlined to code over class @ToLang@. It invokes the method @toLangExpr@ on the value.
-- Note that in this form inside the code value can have any type. For typechecker
-- it's treated just like haskell value @undefined@. So if type in the external code is erroneous
-- program is going to fail at runtime.
--
-- It can be usefull to restrict the types of inlined haskell values
-- to be able to check them at compile-time.
--
-- We can do it with explicit type declaration::
--
-- > [utxo|main = pk ($(alicePubKey) :: PublicKey) |]
--
--  This expression is going to fail at compile time if @alicePubKey@ has inproper type.
--
-- We can inline:
--
--    * primitives: Int, Int64, Bool, Text, ByteString, Sigma ByteString, Script (inlined as ByteString),
--                  PublicKey (inlined as ByteString),
--
--    * composites: lists and tuples of inlineable values.
--
-- Useful examples:
--
--  Simple spends:
--
-- > spendScript = [utxo|pk $(alicePubKey)|]
--
--  Spend with delay time:
--
-- > spendDelayed delay alicePk bobPk = [|utxo
-- >
-- >    main =   (pk (alicePk) &&* toSigma (getHeight < (delay)))
-- >         ||* (pk (bobPk)   &&* toSigma (getHeight >= (delay)))
-- > |]
module Hschain.Utxo.Lang.Parser.Quoter(
    utxo
  , utxoModule
) where

import Control.Monad.Writer.Strict
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail (MonadFail)
#endif

import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH
import Language.Haskell.TH.Quote
import Instances.TH.Lift ()

import Data.Fix

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Generics

import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Exec.Module
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Build (Expr(..))
import Hschain.Utxo.Lang.Module
import Hschain.Utxo.Lang.Sigma (Sigma, PublicKey, ProofInput)
import Hschain.Utxo.Lang.Types (Script)
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base (baseLibInferCtx, baseLibTypeContext)
import Hschain.Utxo.Lang.Parser.Hask
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Build (mainExprModule)
import Hschain.Utxo.Lang.Const (intT', boolT', bytesT', textT', sigmaT', listT', tupleT')

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.Haskell.Exts.SrcLoc as P
import qualified Type.Check.HM as H

-- | Creates values of type Script from quasi-quoted code
utxo :: QuasiQuoter
utxo = QuasiQuoter
  { quoteExp  = defQuoteScript
  , quotePat  = noDef "patterns"
  , quoteType = noDef "types"
  , quoteDec  = noDef "declarations"
  }

utxoModule :: QuasiQuoter
utxoModule = QuasiQuoter
  { quoteExp  = defQuoteModule
  , quotePat  = noDef "patterns"
  , quoteType = noDef "types"
  , quoteDec  = noDef "declarations"
  }

noDef :: String -> a
noDef x = error $ "Quasi-quoting is not defined for " <> x

defQuoteScript :: String -> TH.Q TH.Exp
defQuoteScript str = do
  modExpr <- defQuoteModule str
  [|toCoreScriptUnsafe $(pure modExpr)|]

defQuoteModule :: String -> TH.ExpQ
defQuoteModule str = do
  pos <- getPosition
  expr <- parseScript pos str
  modExpr <- dataToExpQ ( const Nothing
                `extQ` antiQuoteVar
                `extQ` (fromLift @ByteString)
                `extQ` (fromLift @Text)
                `extQ` (fromLift @ProofInput)
                `extQ` (fromLift @PublicKey)
             ) expr
  return modExpr
  where
    getPosition = fmap transPos TH.location
      where
        transPos loc = ( TH.loc_filename loc
                        , fst (TH.loc_start loc)
                        , snd (TH.loc_start loc))

parseScript :: (String, Int, Int) -> String -> TH.Q Module
parseScript pos@(file, _, _) str =
  case parseExp (Just file) str of
    ParseOk expr -> fmap (mainExprModule . Expr) $ typeCheckExpr expr
    ParseFailed _ _ ->
      case parseModule (Just file) str of
        ParseOk m           -> typeCheckModule m
        ParseFailed loc err -> failBy $ shiftError pos $ ParseError (fromParserLoc loc) (T.pack err)
  where
    typeCheckExpr e = do
      let (e', externalCtx) = substAntiQuoteExpr e
      void $ checkError pos $ runInferM $ inferExpr (baseLibInferCtx <> InferCtx externalCtx mempty) e'
      return e

    typeCheckModule m = do
      let (m', externalCtx) = substAntiQuoteModule m
      void $ checkError pos $ evalModule (baseLibTypeContext <> externalCtx) m'
      return m

checkError :: (MonadFail m) => (String, Int, Int) -> Either Error a -> m a
checkError pos = either (failBy . shiftError pos) pure

failBy :: MonadFail m => Error -> m a
failBy err = fail $ T.unpack $ renderText err

fromLift :: TH.Lift a => a -> Maybe TH.ExpQ
fromLift x = Just [| x |]

antiQuoteVar :: E Lang -> Maybe TH.ExpQ
antiQuoteVar = \case
   AntiQuote loc mty v -> Just $ do
     let nameExpr = TH.mkName $ T.unpack $ varName'name v
     case mty of
       Just ty -> [|toLangExpr $(dataToExpQ (const Nothing) loc) ($(TH.varE nameExpr) :: $(quoteToHaskType ty))|]
       Nothing -> [|toLangExpr $(dataToExpQ (const Nothing) loc) $(TH.varE nameExpr)|]
   _                 -> Nothing

substAntiQuoteExpr :: Lang -> (Lang, TypeContext)
substAntiQuoteExpr lang = runWriter $ substAntiQuoteLang lang

substAntiQuoteLang :: Lang -> Writer TypeContext Lang
substAntiQuoteLang lang = foldFixM go lang
  where
    go = \case
      AntiQuote loc mty v -> do
        tell $ H.Context $ M.singleton (varName'name v) $ maybe (anyType loc) (H.monoT . quoteToType loc) mty
        return $ Fix $ Var loc v
      other -> pure $ Fix other

    anyType loc = H.forAllT loc "a" $ H.monoT $ H.varT loc "a"

quoteToType :: Loc -> QuoteType -> Type
quoteToType loc = \case
  IntQ       -> intT' loc
  BoolQ      -> boolT' loc
  BytesQ     -> bytesT' loc
  TextQ      -> textT' loc
  SigmaQ     -> sigmaT' loc
  PublicKeyQ -> bytesT' loc
  ScriptQ    -> bytesT' loc
  ListQ t    -> listT' loc $ rec t
  TupleQ ts  -> tupleT' loc $ fmap rec ts
  where
    rec = quoteToType loc

quoteToHaskType :: QuoteType -> TH.TypeQ
quoteToHaskType = \case
  IntQ   -> [t|Int|]
  BoolQ  -> [t|Bool|]
  TextQ  -> [t|Text|]
  BytesQ -> [t|ByteString|]
  SigmaQ -> [t|Sigma ByteString|]
  PublicKeyQ -> [t|PublicKey|]
  ScriptQ -> [t|Script|]
  ListQ t -> TH.appT TH.listT $ quoteToHaskType t
  TupleQ ts -> foldl TH.appT (TH.tupleT (length ts)) $ fmap quoteToHaskType ts

substAntiQuoteModule :: Module -> (Module, TypeContext)
substAntiQuoteModule m@Module{..} = (m { module'binds = bs }, ctx)
  where
    (bs, ctx) = runWriter $ mapDeclsM (mapM substAntiQuoteLang) module'binds

shiftError :: (String, Int, Int) -> Error -> Error
shiftError pos = everywhere (mkT (shiftLoc pos))

shiftLoc :: (String, Int, Int) -> Loc -> Loc
shiftLoc (file, line, _col) loc@P.SrcSpanInfo{..} = loc
  { P.srcInfoSpan   = shiftSpan $ P.srcInfoSpan loc
  , P.srcInfoPoints = fmap shiftSpan $ P.srcInfoPoints loc
  }
  where
    shiftSpan x = x
      { P.srcSpanFilename = file
      , P.srcSpanStartLine = line - 1 + P.srcSpanStartLine x
      , P.srcSpanEndLine = line - 1 + P.srcSpanEndLine x
      }



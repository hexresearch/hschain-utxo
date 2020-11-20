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
-- For interpolation of external haskell values we use single parens around variable:
--
-- > [utxo|main = pk (alicePubKey)|]
--
-- Value is inlined to code over class @ToLang@. It invokes the method @toLangExpr@ on the value.
-- Note that in this form inside the code value can have any type. For typechecker
-- it's treated just like haskell value @undefined@. So if type in the external code is erroneous
-- program is going to fail at runtime.
--
-- It can be usefull to restrict the types of inlined haskell values
-- to be able to check them at compile-time.
--
-- We can do it with special form by supplying the type with operator @#@:
--
-- > [utxo|main = pk (alicePubKey # PublicKey) |]
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
-- > spendScript = [utxo|pk (alicePubKey)|]
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
) where

import Control.Monad.Writer.Strict

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.Fix

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Generics

import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Exec.Module
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Sigma (Sigma, PublicKey)
import Hschain.Utxo.Lang.Types (ArgType(..), Script)
import Hschain.Utxo.Lang.Infer
import Hschain.Utxo.Lang.Lib.Base (baseModuleCtx, baseLibTypeContext)
import Hschain.Utxo.Lang.Parser.Hask
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Build (mainExprModule)

import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Language.Haskell.Exts.SrcLoc as P
import qualified Language.HM as H
import qualified Data.ByteString.Char8 as C

utxo :: QuasiQuoter
utxo = QuasiQuoter
  { quoteExp  = defQuoteExp
  , quotePat  = undefined
  , quoteType = undefined
  , quoteDec  = undefined
  }

defQuoteExp :: String -> TH.Q TH.Exp
defQuoteExp str = do
  pos  <- getPosition
  expr <- parseScript pos str
  modExpr <- dataToExpQ ( const Nothing
                `extQ` antiQuoteVar
                `extQ` (Just . fromBS)
                `extQ` (Just . fromText)
             ) expr
  [|toCoreScriptUnsafe $(pure modExpr)|]
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
        ParseFailed loc err -> failBy $ shiftError pos $ ParseError (P.toSrcInfo loc [] loc) (T.pack err)
  where
    typeCheckExpr e = do
      let (e', externalCtx) = substAntiQuoteExpr e
      void $ checkError pos $ runInferM $ inferExpr (moduleCtx'types baseModuleCtx <> InferCtx externalCtx mempty) e'
      return e

    typeCheckModule m = do
      let (m', externalCtx) = substAntiQuoteModule m
      void $ checkError pos $ evalModule (baseLibTypeContext <> externalCtx) m'
      return m

checkError :: (MonadFail m) => (String, Int, Int) -> Either Error a -> m a
checkError pos = either (failBy . shiftError pos) pure

failBy :: MonadFail m => Error -> m a
failBy err = fail $ T.unpack $ renderText err

fromBS :: ByteString -> TH.ExpQ
fromBS x = TH.litE $ TH.StringL $ C.unpack x

fromText :: Text -> TH.ExpQ
fromText txt = TH.litE $ TH.StringL $ T.unpack txt

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
substAntiQuoteLang lang = cataM go lang
  where
    go = \case
      AntiQuote loc mty v -> do
        tell $ H.Context $ M.singleton (varName'name v) $ maybe (anyType loc) (H.monoT . quoteToType loc) mty
        return $ Fix $ Var loc v
      other -> pure $ Fix other

    anyType loc = H.forAllT loc "a" $ H.monoT $ H.varT loc "a"

quoteToType :: Loc -> QuoteType -> Type
quoteToType loc = \case
  PrimQ ty   -> argTagToType' loc ty
  SigmaQ     -> sigmaT' loc
  PublicKeyQ -> bytesT' loc
  ScriptQ    -> bytesT' loc
  ListQ t    -> listT' loc $ rec t
  TupleQ ts  -> tupleT' loc $ fmap rec ts
  where
    rec = quoteToType loc

quoteToHaskType :: QuoteType -> TH.TypeQ
quoteToHaskType = \case
  PrimQ t -> case t of
    IntArg   -> [t|Int|]
    BoolArg  -> [t|Bool|]
    TextArg  -> [t|Text|]
    BytesArg -> [t|ByteString|]
  SigmaQ -> [t|Sigma ByteString|]
  PublicKeyQ -> [t|PublicKey|]
  ScriptQ -> [t|Script|]
  ListQ t -> TH.appT TH.listT $ quoteToHaskType t
  TupleQ ts -> foldl TH.appT (TH.tupleT (length ts)) $ fmap quoteToHaskType ts

substAntiQuoteModule :: Module -> (Module, TypeContext)
substAntiQuoteModule m@Module{..} = (m { module'binds = bs }, ctx)
  where
    (bs, ctx) = runWriter $ mapM substBind module'binds

    substBind b = mapM substAntiQuoteLang b

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



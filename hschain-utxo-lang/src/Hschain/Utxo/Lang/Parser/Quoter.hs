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
   AntiQuote loc _ v -> Just $ do
     let nameExpr = TH.mkName $ T.unpack $ varName'name v
     [|toLangExpr $(dataToExpQ (const Nothing) loc) $(TH.varE nameExpr)|]
   _                 -> Nothing

substAntiQuoteExpr :: Lang -> (Lang, TypeContext)
substAntiQuoteExpr lang = runWriter $ substAntiQuoteLang lang

substAntiQuoteLang :: Lang -> Writer TypeContext Lang
substAntiQuoteLang lang = cataM go lang
  where
    go = \case
      AntiQuote loc mty v -> do
        tell $ H.Context $ M.singleton (varName'name v) $ maybe (anyType loc) (H.monoT . argTagToType' loc) mty
        return $ Fix $ Var loc v
      other -> pure $ Fix other

    anyType loc = H.forAllT loc "a" $ H.monoT $ H.varT loc "a"

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



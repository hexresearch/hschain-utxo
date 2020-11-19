module Hschain.Utxo.Lang.Parser.Quoter(
    utxo
) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Generics

import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Parser.Hask
import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.Lang.Build (mainExprModule)

import qualified Data.Text as T
import qualified Language.Haskell.Exts.SrcLoc as P
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

parseScript :: (MonadFail m, Monad m) => (String, Int, Int) -> String -> m Module
parseScript (file, _line, _col) str =
  case parseExp (Just file) str of
    ParseOk expr -> pure $ mainExprModule $ Expr expr
    ParseFailed _ _ ->
      case parseModule (Just file) str of
        ParseOk m           -> pure m
        ParseFailed loc err -> failBy $ ParseError (P.toSrcInfo loc [] loc) (T.pack err)

failBy :: MonadFail m => Error -> m a
failBy err = fail $ T.unpack $ renderText err

fromBS :: ByteString -> TH.ExpQ
fromBS x = TH.litE $ TH.StringL $ C.unpack x

fromText :: Text -> TH.ExpQ
fromText txt = TH.litE $ TH.StringL $ T.unpack txt

antiQuoteVar :: E Lang -> Maybe TH.ExpQ
antiQuoteVar = \case
   AntiQuote loc _ v -> Just $ do
     nameExpr <- TH.varE (TH.mkName $ T.unpack $ varName'name v)
     [|toLangExpr $(dataToExpQ (const Nothing) loc) $(pure nameExpr)|]
   _                 -> Nothing


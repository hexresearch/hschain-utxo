module Hschain.Utxo.Lang.Parser.Quoter(
  utxo
) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.ByteString (ByteString)
-- import Data.Typeable (cast)

import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Error
import Hschain.Utxo.Lang.Expr
import Hschain.Utxo.Lang.Types
import Hschain.Utxo.Lang.Parser.Hask
import Hschain.Utxo.Lang.Pretty

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
  fromModule expr
--  dataToExpQ (fmap fromBS . cast) expr
  where
    getPosition = fmap transPos TH.location
      where
        transPos loc = ( TH.loc_filename loc
                       , fst (TH.loc_start loc)
                       , snd (TH.loc_start loc))

parseScript :: (MonadFail m, Monad m) => (String, Int, Int) -> String -> m Module
parseScript (file, _line, _col) str =
  case parseModule (Just file) str of
    ParseOk m           -> pure m
    ParseFailed loc err -> failBy $ ParseError (P.toSrcInfo loc [] loc) (T.pack err)
  where
    failToCompile err = failBy err

toScript :: (MonadFail m) => Module -> m Script
toScript m = either failToCompile pure $ toCoreScript m
  where
    failToCompile err = failBy err


failBy :: MonadFail m => Error -> m a
failBy err = fail $ T.unpack $ renderText err

fromBS :: ByteString -> TH.ExpQ
fromBS x = TH.litE $ TH.StringL $ C.unpack x

------------------------------------------------

fromModule :: Module -> TH.ExpQ
fromModule Module{..} =
  [| Module  $(fromLoc module'loc) $(fromUserTypeCtx module'userTypes) $(fmap TH.ListE $ mapM fromBind module'binds) |]

fromLoc :: Loc -> TH.ExpQ
fromLoc = dataToExpQ (const Nothing)

fromUserTypeCtx :: UserTypeCtx -> TH.ExpQ
fromUserTypeCtx = dataToExpQ (const Nothing)

fromBind :: Bind Lang -> TH.ExpQ
fromBind = undefined

fromVarName :: VarName -> TH.ExpQ
fromVarName = dataToExpQ (const Nothing)


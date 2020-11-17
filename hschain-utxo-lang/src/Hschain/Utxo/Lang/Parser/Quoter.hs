module Hschain.Utxo.Lang.Parser.Quoter(
  utxo
) where

import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.ByteString (ByteString)
import Data.Typeable (cast)

import Hschain.Utxo.Lang.Compile
import Hschain.Utxo.Lang.Error
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
  dataToExpQ (fmap fromBS . cast) expr
  where
    getPosition = fmap transPos TH.location
      where
        transPos loc = ( TH.loc_filename loc
                       , fst (TH.loc_start loc)
                       , snd (TH.loc_start loc))

parseScript :: (MonadFail m, Monad m) => (String, Int, Int) -> String -> m Script
parseScript (file, _line, _col) str =
  case parseModule (Just file) str of
    ParseOk m           -> either failToCompile pure $ toCoreScript m
    ParseFailed loc err -> failBy $ ParseError (P.toSrcInfo loc [] loc) (T.pack err)
  where
    failToCompile err = failBy err

failBy :: MonadFail m => Error -> m a
failBy err = fail $ T.unpack $ renderText err

fromBS :: ByteString -> TH.ExpQ
fromBS x = TH.litE $ TH.StringL $ C.unpack x


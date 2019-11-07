module Hschain.Utxo.State.Pretty(
) where

import Data.Text.Prettyprint.Doc

import Hschain.Utxo.Lang.Pretty
import Hschain.Utxo.State.Types

import qualified Data.Map.Strict as M

instance Pretty BoxChain where
  pretty BoxChain{..} = prettyRecord "BoxChain"
    [ ("height", pretty boxChain'height)
    , ("boxes",  vsep $ fmap pretty $ M.elems boxChain'boxes )]


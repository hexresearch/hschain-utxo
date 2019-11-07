module Hex.Common.Url(
    toUrl
  , toUrlHttps
) where

import Data.Text (Text)
import Network.URL

import qualified Data.Text as T

toUrl :: Text -> Int -> Text
toUrl host port = toUrlBy False host port

toUrlHttps :: Text -> Int -> Text
toUrlHttps host port = toUrlBy True host port

toUrlBy :: Bool -> Text -> Int -> Text
toUrlBy isHttps host port =
  T.pack $ exportURL $
    URL (Absolute (Host (HTTP isHttps) (T.unpack host) (Just $ fromIntegral port))) "" []


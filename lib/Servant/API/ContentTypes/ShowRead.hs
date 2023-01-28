{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.ShowRead where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Control.Monad ((<=<))
import Data.Text.Lazy (pack, unpack)
import Data.Text.Lazy.Encoding (encodeUtf8, decodeUtf8')
import Text.Read (readEither)

import Servant.API.ContentTypes

-- | Content type for UTF-8 encoded data produced by a 'Show' instance and
-- parsed by a 'Read' instance. This is probably slow.
data ShowReadCt

-- | Mime type using the phrases "haskell" and "showread".
instance Accept ShowReadCt where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-haskell-showread"
        , "application" // "vnd.haskell.showread"
        ]

instance Show a => MimeRender ShowReadCt a where
    mimeRender Proxy = encodeUtf8 . pack . show

instance Read a => MimeUnrender ShowReadCt a where
    mimeUnrender Proxy = readEither . unpack <=< mapLeft show . decodeUtf8'
      where
        mapLeft f = either (Left . f) Right

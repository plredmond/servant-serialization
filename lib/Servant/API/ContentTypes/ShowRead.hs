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

-- | Content-type for instances of the 'Show' and 'Read' classes encoded as
-- UTF-8 data. This is probably slow.
data ShowRead

-- | Mime-type using the phrases "haskell" and "showread".
instance Accept ShowRead where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-haskell-showread"
        , "application" // "vnd.haskell.showread"
        ]

instance Show a => MimeRender ShowRead a where
    mimeRender Proxy = encodeUtf8 . pack . show

instance Read a => MimeUnrender ShowRead a where
    mimeUnrender Proxy = readEither . unpack <=< mapLeft show . decodeUtf8'
      where
        mapLeft f = either (Left . f) Right

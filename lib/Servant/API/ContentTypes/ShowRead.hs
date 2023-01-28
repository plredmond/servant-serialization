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
import Data.Text.Encoding.Error (UnicodeException(..))
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

-- $setup
-- >>> :set -XOverloadedStrings

-- | Decode UTF-8 data and then with 'Read' instance.
--
-- >>> mimeUnrender (Proxy :: Proxy ShowRead) "1e5" :: Either String Double
-- Right 100000.0
--
-- >>> mimeUnrender (Proxy :: Proxy ShowRead) "hello" :: Either String Double
-- Left "Prelude.read: no parse"
--
-- >>> mimeUnrender (Proxy :: Proxy ShowRead) "hello\xc3\x28" :: Either String Double
-- Left "Data.Text.Internal.Encoding.streamDecodeUtf8With: Invalid UTF-8 stream"
instance Read a => MimeUnrender ShowRead a where
    mimeUnrender Proxy = readEither . unpack <=< mapLeft prettyErr . decodeUtf8'
      where
        mapLeft f = either (Left . f) Right
        prettyErr (DecodeError err _invalidByteValMaybe) = err
        prettyErr _ = "unknown error" -- TODO: when 'text' removes deprecated 'EncodeError' constructor, remove this case

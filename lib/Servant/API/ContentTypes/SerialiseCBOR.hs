{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.SerialiseCBOR where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Codec.Serialise

-- | Content-type for instances of the 'Serialise' class in the package
-- "serialise".
data CBOR

-- | Mime-type for CBOR and additional ones using the word "hackage" and the
-- name of the package "serialise".
instance Accept CBOR where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "cbor"
        , "application" // "x-hackage-binary"
        , "application" // "vnd.hackage.binary"
        ]

instance Serialise a => MimeRender CBOR a where
    mimeRender Proxy = serialise

instance Serialise a => MimeUnrender CBOR a where
    mimeUnrender Proxy = mapLeft show . deserialiseOrFail
      where
        mapLeft f = either (Left . f) Right

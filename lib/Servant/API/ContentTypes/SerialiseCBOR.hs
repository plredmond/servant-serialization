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
-- "serialise". Trailing garbage is ignored.
data CBOR

-- | Mime-type for CBOR and additional ones using the word "hackage" and the
-- name of the package "serialise".
instance Accept CBOR where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "cbor"
        , "application" // "x-hackage-serialise"
        , "application" // "vnd.hackage.serialise"
        ]

-- |
--
-- >>> mimeRender (Proxy :: Proxy CBOR) (3.14 :: Float)
-- "\250@H\245\195"
instance Serialise a => MimeRender CBOR a where
    mimeRender Proxy = serialise

-- |
--
-- >>> let bsl = mimeRender (Proxy :: Proxy CBOR) (3.14 :: Float)
-- >>> mimeUnrender (Proxy :: Proxy CBOR) bsl :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy CBOR) (bsl <> "trailing garbage") :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy CBOR) ("preceding garbage" <> bsl) :: Either String Float
-- Left "Codec.Serialise.deserialiseOrFail: expected float at byte-offset 0"
instance Serialise a => MimeUnrender CBOR a where
    mimeUnrender Proxy = mapLeft prettyErr . deserialiseOrFail
      where
        mapLeft f = either (Left . f) Right
        prettyErr (DeserialiseFailure offset err) =
            "Codec.Serialise.deserialiseOrFail: " ++ err ++ " at byte-offset " ++ show offset

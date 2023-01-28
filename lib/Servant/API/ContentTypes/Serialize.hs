{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.Serialize where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Data.Serialize

-- | Content type for instances of the 'Serialize' class.
data SerializeCt

-- | Mime type using the word "hackage" and the name of the package "cereal".
instance Accept SerializeCt where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-hackage-cereal"
        , "application" // "vnd.hackage.cereal"
        ]

instance Serialize a => MimeRender SerializeCt a where
    mimeRender Proxy = encodeLazy

instance Serialize a => MimeUnrender SerializeCt a where
    mimeUnrender Proxy = decodeLazy

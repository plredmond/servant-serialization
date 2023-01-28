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
data Cereal

-- | Mime type using the word "hackage" and the name of the package "cereal".
instance Accept Cereal where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-hackage-cereal"
        , "application" // "vnd.hackage.cereal"
        ]

instance Serialize a => MimeRender Cereal a where
    mimeRender Proxy = encodeLazy

instance Serialize a => MimeUnrender Cereal a where
    mimeUnrender Proxy = decodeLazy

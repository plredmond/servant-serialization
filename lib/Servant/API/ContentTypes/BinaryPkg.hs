{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.BinaryPkg where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Data.Binary

-- | Content type for instances of the 'Binary' class. Trailing garbage is
-- ignored.
data BinaryPkg

-- | Mime type using the word "hackage" and the name of the package "binary".
instance Accept BinaryPkg where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-hackage-binary"
        , "application" // "vnd.hackage.binary"
        ]

instance Binary a => MimeRender BinaryPkg a where
    mimeRender Proxy = encode

instance Binary a => MimeUnrender BinaryPkg a where
    mimeUnrender Proxy bsl =
        case decodeOrFail bsl of
            Left (_unconsumedInput, _consumedByteCt, err) -> Left err
            Right (_unconsumedInput, _consumedByteCt, val) -> Right val

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.Binary where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Data.Binary

-- | Content type for instances of the 'Binary' class. Trailing garbage is
-- ignored.
data BinaryCt

instance Accept BinaryCt where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-haskell-binary"
        , "application" // "vnd.haskell.binary"
        ]

instance Binary a => MimeRender BinaryCt a where
    mimeRender Proxy = encode

instance Binary a => MimeUnrender BinaryCt a where
    mimeUnrender Proxy bsl =
        case decodeOrFail bsl of
            Left (_unconsumedInput, _consumedByteCt, err) -> Left err
            Right (_unconsumedInput, _consumedByteCt, val) -> Right val

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.BinaryPkg where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Data.Binary

-- | Content-type for instances of the 'Binary' class in the package "binary".
-- Trailing garbage is ignored.
data BinaryPkg

-- | Mime-type using the word "hackage" and the name of the package "binary".
instance Accept BinaryPkg where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-hackage-binary"
        , "application" // "vnd.hackage.binary"
        ]

-- |
--
-- >>> mimeRender (Proxy :: Proxy BinaryPkg) (3.14 :: Float)
-- "\NUL\NUL\200\245\195\255\255\255\255\255\255\255\234"
instance Binary a => MimeRender BinaryPkg a where
    mimeRender Proxy = encode

-- |
--
-- >>> let bsl = mimeRender (Proxy :: Proxy BinaryPkg) (3.14 :: Float)
-- >>> mimeUnrender (Proxy :: Proxy BinaryPkg) bsl :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy BinaryPkg) (bsl <> "trailing garbage") :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy BinaryPkg) ("preceding garbage" <> bsl) :: Either String Float
-- Left "Data.Binary.decodeOrFail: not enough bytes at byte-offset 30"
instance Binary a => MimeUnrender BinaryPkg a where
    mimeUnrender Proxy bsl =
        case decodeOrFail bsl of
            Left (_unconsumedInput, consumedByteCt, err) -> Left $ "Data.Binary.decodeOrFail: " ++ err ++ " at byte-offset " ++ show consumedByteCt
            Right (_unconsumedInput, _consumedByteCt, val) -> Right val

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.Cereal where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Data.Serialize

-- | Content-type for instances of the 'Serialize' class in the package
-- "cereal". Trailing garbage is ignored.
data Cereal

-- | Mime-type using the word "hackage" and the name of the package "cereal".
instance Accept Cereal where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-hackage-cereal"
        , "application" // "vnd.hackage.cereal"
        ]

-- |
--
-- >>> mimeRender (Proxy :: Proxy Cereal) (3.14 :: Float)
-- "@H\245\195"
instance Serialize a => MimeRender Cereal a where
    mimeRender Proxy = encodeLazy

-- |
--
-- >>> let bsl = mimeRender (Proxy :: Proxy Cereal) (3.14 :: Float)
-- >>> mimeUnrender (Proxy :: Proxy Cereal) bsl :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy Cereal) (bsl <> "trailing garbage") :: Either String Float
-- Right 3.14
--
-- Cereal doesn't detect this preceding garbage.
--
-- >>> mimeUnrender (Proxy :: Proxy Cereal) ("preceding garbage" <> bsl) :: Either String Float
-- Right ...
--
-- >>> mimeUnrender (Proxy :: Proxy Cereal) "garbage" :: Either String (Float, Float)
-- Left "Data.Serialize.decodeLazy: too few bytes\nFrom:\tdemandInput\n\n"
instance Serialize a => MimeUnrender Cereal a where
    mimeUnrender Proxy = mapLeft ("Data.Serialize.decodeLazy: " ++) . decodeLazy
      where
        mapLeft f = either (Left . f) Right

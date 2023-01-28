{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.Persist where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Data.Persist
import qualified Data.ByteString.Lazy as BSL (toStrict, fromStrict)

-- | Content-type for instances of the 'Persist' class in the package
-- "persist". Trailing garbage is ignored.
data PersistFmt

-- | Mime-type using the word "hackage" and the name of the package "persist".
instance Accept PersistFmt where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-hackage-persist"
        , "application" // "vnd.hackage.persist"
        ]

-- |
--
-- >>> mimeRender (Proxy :: Proxy PersistFmt) (3.14 :: Float)
-- "\195\245H@"
instance Persist a => MimeRender PersistFmt a where
    mimeRender Proxy = BSL.fromStrict . encode

-- |
--
-- >>> let bsl = mimeRender (Proxy :: Proxy PersistFmt) (3.14 :: Float)
-- >>> mimeUnrender (Proxy :: Proxy PersistFmt) bsl :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy PersistFmt) (bsl <> "trailing garbage") :: Either String Float
-- Right 3.14
--
-- Persist doesn't detect this preceding garbage.
--
-- >>> mimeUnrender (Proxy :: Proxy PersistFmt) ("preceding garbage" <> bsl) :: Either String Float
-- Right ...
--
-- >>> mimeUnrender (Proxy :: Proxy PersistFmt) "garbage" :: Either String (Float, Float)
-- Left "Data.Persist.decode: LengthException 4 \"Not enough bytes available\""
instance Persist a => MimeUnrender PersistFmt a where
    mimeUnrender Proxy = mapLeft ("Data.Persist.decode: " ++) . decode . BSL.toStrict
      where
        mapLeft f = either (Left . f) Right

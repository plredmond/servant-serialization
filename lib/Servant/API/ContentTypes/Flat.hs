{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Servant.API.ContentTypes.Flat where

import Data.Proxy (Proxy(..))
import Network.HTTP.Media ((//))
import qualified Data.List.NonEmpty as NonEmpty

import Servant.API.ContentTypes
import Flat

-- | Content-type for instances of the 'Flat' class in the package
-- "flat".
data FlatFmt

-- | Mime-type using the word "hackage" and the name of the package "flat".
instance Accept FlatFmt where
    contentTypes Proxy = NonEmpty.fromList
        [ "application" // "x-hackage-flat"
        , "application" // "vnd.hackage.flat"
        ]

-- |
--
-- >>> mimeRender (Proxy :: Proxy FlatFmt) (3.14 :: Float)
-- "@H\245\195"
instance Flat a => MimeRender FlatFmt a where
    mimeRender Proxy = flatRaw

-- |
--
-- >>> let bsl = mimeRender (Proxy :: Proxy FlatFmt) (3.14 :: Float)
-- >>> mimeUnrender (Proxy :: Proxy FlatFmt) bsl :: Either String Float
-- Right 3.14
--
-- >>> mimeUnrender (Proxy :: Proxy FlatFmt) (bsl <> "trailing garbage") :: Either String Float
-- Left "Flat.unflatRaw: TooMuchSpace (0x...,S {currPtr = 0x..., usedBits = 0})"
--
-- >>> mimeUnrender (Proxy :: Proxy FlatFmt) ("preceding garbage" <> bsl) :: Either String Float
-- Left "Flat.unflatRaw: TooMuchSpace (0x...,S {currPtr = 0x..., usedBits = 0})"
instance Flat a => MimeUnrender FlatFmt a where
    mimeUnrender Proxy = mapLeft prettyErr . unflatRaw
      where
        mapLeft f = either (Left . f) Right
        prettyErr err = "Flat.unflatRaw: " ++ show err

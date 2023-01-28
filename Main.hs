{-# OPTIONS_GHC "-Wno-missing-signatures" #-}
{-# OPTIONS_GHC "-Wno-unused-top-binds" #-}
{-# OPTIONS_GHC "-Wno-orphans" #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Proxy (Proxy(..))
import GHC.Generics (Generic)
import Network.Wai.Handler.Warp (run)
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Web.FormUrlEncoded as UrlEncoded (ToForm, FromForm)
import qualified Data.Aeson as Aeson (ToJSON, FromJSON)
import qualified Data.Binary as Binary (Binary)
import qualified Data.Serialize as Cereal (Serialize)

import Servant.API
import Servant.Server
import Servant.API.ContentTypes.ShowRead
import Servant.API.ContentTypes.BinaryPkg
import Servant.API.ContentTypes.Cereal

import Servant.Client
import Network.HTTP.Client (Manager)
import qualified Test.QuickCheck as QC

data Example = Example
    { slythy :: Bool
    , momeRaths :: [Either Char Ordering]
    , grabe :: Int
    } deriving (Eq, Show, Read, Generic)

instance UrlEncoded.FromForm Example
instance UrlEncoded.ToForm Example

instance Aeson.FromJSON Example
instance Aeson.ToJSON Example

instance Binary.Binary Example

instance Cereal.Serialize Example

type TestAPI a
    -- string types
    =    "string"   :> ReqBody '[PlainText] String              :> Post '[PlainText] String
    :<|> "text"     :> ReqBody '[PlainText] T.Text              :> Post '[PlainText] T.Text
    :<|> "bs"       :> ReqBody '[OctetStream] BS.ByteString     :> Post '[OctetStream] BS.ByteString
    :<|> "bsl"      :> ReqBody '[OctetStream] BSL.ByteString    :> Post '[OctetStream] BSL.ByteString
    -- builtin serialization
    :<|> "urlenc"   :> ReqBody '[FormUrlEncoded] a              :> Post '[FormUrlEncoded] a
    :<|> "json"     :> ReqBody '[JSON] a                        :> Post '[JSON] a
    -- additional serialization
    :<|> "showread" :> ReqBody '[ShowRead] a                    :> Post '[ShowRead] a
    :<|> "binary"   :> ReqBody '[BinaryPkg] a                   :> Post '[BinaryPkg] a
    :<|> "cereal"   :> ReqBody '[Cereal] a                      :> Post '[Cereal] a

-- | Client functions
rtString
  :<|> rtText
  :<|> rtBS
  :<|> rtBSL
  :<|> rtUrlEnc
  :<|> rtJson
  :<|> rtShow
  :<|> rtBinary
  :<|> rtCereal
    = client (Proxy @(TestAPI Example))

main :: IO ()
main
    = run 80801
    . serve (Proxy @(TestAPI Example))
    $    return
    :<|> return
    :<|> return
    :<|> return
    :<|> return
    :<|> return
    :<|> return
    :<|> return
    :<|> return

-- $setup
-- >>> import Control.Concurrent (forkIO)
-- >>> forkIO main -- run a server in the background; BE SURE TO TEARDOWN
-- ThreadId ...
-- >>> import Network.HTTP.Client (newManager, defaultManagerSettings)
-- >>> mgr <- newManager defaultManagerSettings
-- >>> import Test.QuickCheck.Monadic (monadicIO)

-- | Send a value through a ClientM action and report whether it is unchanged.
--
-- !!!!> \x -> QC.ioProperty $ testRoundTrip mgr rtString   x
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtText     x
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtBS       x
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtBSL      x
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtUrlEnc   (x :: Example)
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtJson     (x :: Example)
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtShow     (x :: Example)
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtBinary   (x :: Example)
-- prop> \x -> QC.ioProperty $ testRoundTrip mgr rtCereal   (x :: Example)
testRoundTrip :: (Eq a, Show a) => Manager -> (a -> ClientM a) -> a -> IO Bool
testRoundTrip mgr roundtrip val = do
    let env = mkClientEnv mgr $ BaseUrl Http "localhost" 80801 ""
    result <- runClientM (roundtrip val) env
    case result of
        Left err -> print err >> return False
        Right val2
            | val == val2 -> return True
            | otherwise -> do
                putStrLn $ show val ++ " → " ++ show val2
                return False

instance QC.Arbitrary T.Text where
    arbitrary = T.pack <$> QC.arbitrary
    shrink = fmap T.pack . QC.shrink . T.unpack

instance QC.Arbitrary BS.ByteString where
    arbitrary = BS.pack <$> QC.arbitrary
    shrink = fmap BS.pack . QC.shrink . BS.unpack

instance QC.Arbitrary BSL.ByteString where
    arbitrary = BSL.pack <$> QC.arbitrary
    shrink = fmap BSL.pack . QC.shrink . BSL.unpack

instance QC.Arbitrary Example where
    arbitrary = Example <$> QC.arbitrary <*> QC.arbitrary <*> QC.arbitrary
    shrink (Example a b c) =
        [ Example a' b' c'
        | a' <- QC.shrink a
        , b' <- QC.shrink b
        , c' <- QC.shrink c
        ]

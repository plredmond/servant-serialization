{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DeriveGeneric #-}

import Data.Proxy (Proxy(..))
import Network.Wai.Handler.Warp (run)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Web.FormUrlEncoded as UrlEncoded (ToForm, FromForm)
import qualified Data.Aeson as Aeson (ToJSON, FromJSON)

import Servant.API
import Servant.Server
import Servant.API.ContentTypes.ShowRead

data Example = Example
    { slythy :: Bool
    , momeRaths :: [Either Char Ordering]
    , grabe :: Int
    } deriving (Generic, Show, Read)

instance UrlEncoded.FromForm Example
instance UrlEncoded.ToForm Example

instance Aeson.FromJSON Example
instance Aeson.ToJSON Example

type TestAPI a
    -- string types
    =    "string"   :> ReqBody '[PlainText] String              :> Post '[PlainText] String
    :<|> "text"     :> ReqBody '[PlainText] Text                :> Post '[PlainText] Text
    :<|> "bs"       :> ReqBody '[OctetStream] BS.ByteString     :> Post '[OctetStream] BS.ByteString
    :<|> "bsl"      :> ReqBody '[OctetStream] BSL.ByteString    :> Post '[OctetStream] BSL.ByteString
    -- builtin serialization
    :<|> "urlenc"   :> ReqBody '[FormUrlEncoded] a              :> Post '[FormUrlEncoded] a
    :<|> "json"     :> ReqBody '[JSON] a                        :> Post '[JSON] a
    -- additional serialization
    :<|> "show"     :> ReqBody '[ShowRead] a                    :> Post '[ShowRead] a

main :: IO ()
main
    = run 8080
    . serve (Proxy @(TestAPI Example))
    $    return
    :<|> return
    :<|> return
    :<|> return
    :<|> return
    :<|> return
    :<|> return

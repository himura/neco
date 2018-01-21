{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Network.RIO.Filter.Request.OAuth2
    ( ClientSetting(..)
    , AccessToken(..)
    , authorizeUri
    , getAccessToken
    , oauth2RequestFilter
    , setOAuth2AuthzHeader
    ) where

import Control.Exception
import Data.Aeson
import qualified Data.ByteString.Char8 as S8
import Data.Char
import Data.Monoid
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import GHC.Generics
import Network.HTTP.Client
import qualified Network.HTTP.Types as HT
import Network.RIO.Filter.Response.JSON
import Network.RIO.Service.HttpClient
import Network.RIO.Types

data ClientSetting =
    ClientSetting
    { oauth2ClientId :: T.Text
    , oauth2ClientSecret :: T.Text
    , oauth2AuthUri :: T.Text
    , oauth2TokenUri :: T.Text
    } deriving (Eq, Show, Read, Generic)
instance FromJSON ClientSetting where
    parseJSON = genericParseJSON genericJSONOptions
instance ToJSON ClientSetting where
    toJSON = genericToJSON genericJSONOptions

data AccessToken = AccessToken
    { oauth2AccessToken :: String
    , oauth2ExpiresIn :: Int
    , oauth2RefleshToken :: String
    , oauth2TokenType :: String
    } deriving (Eq, Show, Read, Generic)
instance FromJSON AccessToken where
    parseJSON = genericParseJSON genericJSONOptions
instance ToJSON AccessToken where
    toJSON = genericToJSON genericJSONOptions

genericJSONOptions :: Options
genericJSONOptions =
    defaultOptions
    {fieldLabelModifier = camelToSnakeCaseWithCommonPrefix "oauth2"}

camelToSnakeCase :: String -> String
camelToSnakeCase [] = []
camelToSnakeCase (x:xs)
    | isUpper x = '_' : toLower x : camelToSnakeCase xs
    | otherwise = x : camelToSnakeCase xs

camelToSnakeCaseWithCommonPrefix :: String -> String -> String
camelToSnakeCaseWithCommonPrefix prefix =
    camelToSnakeCase . lower1st . drop (length prefix)
  where
    lower1st (x:xs) = toLower x : xs
    lower1st [] = []

authorizeUri ::
       ClientSetting
    -> [T.Text] -- ^ OAuth2 scope list
    -> T.Text -- ^ redirect uri
    -> S8.ByteString
authorizeUri ClientSetting {..} scopes redirectUri =
    T.encodeUtf8 oauth2AuthUri <> HT.renderSimpleQuery True q
  where
    q =
        [ ("response_type", "code")
        , ("redirect_uri", T.encodeUtf8 redirectUri)
        , ("scope", T.encodeUtf8 $ T.intercalate "+" scopes)
        , ("client_id", T.encodeUtf8 oauth2ClientId)
        ]

getAccessToken ::
       ClientSetting
    -> T.Text -- ^ redirect uri
    -> T.Text -- ^ grant type
    -> S8.ByteString -- ^ code
    -> Manager
    -> IO AccessToken
getAccessToken ClientSetting {..} redirectUri grantType code mgr = do
    req <- parseUrlThrow $ T.unpack oauth2TokenUri
    runService service (urlEncodedBody params req) $ \res ->
        case responseBody res of
            Right body -> return body
            Left err -> throwIO err
  where
    service = fromJSONResponseFilter $ httpClientService mgr
    params =
        [ ("client_id", T.encodeUtf8 oauth2ClientId)
        , ("client_secret", T.encodeUtf8 oauth2ClientSecret)
        , ("grant_type", T.encodeUtf8 grantType)
        , ("redirect_uri", T.encodeUtf8 redirectUri)
        , ("code", code)
        ]

oauth2RequestFilter :: AccessToken -> Filter m Request Request res res
oauth2RequestFilter accessToken =
    makeRequestFilter $ setOAuth2AuthzHeader accessToken

setOAuth2AuthzHeader :: AccessToken -> Request -> Request
setOAuth2AuthzHeader token req =
    req {requestHeaders = ("Authorization", S8.pack v) : requestHeaders req}
  where
    v = "Bearer " ++ oauth2AccessToken token

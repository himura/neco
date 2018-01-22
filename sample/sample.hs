{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Control.Lens
import Data.Aeson
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Network.RIO
import Network.RIO.Filter.Request.OAuth1
import Web.Twitter.Types.Lens

twitterOAuth :: OAuth
twitterOAuth =
    def { oauthServerName = "twitter"
        , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
        , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
        , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
        , oauthConsumerKey = error "You MUST specify oauthConsumerKey parameter."
        , oauthConsumerSecret = error "You MUST specify oauthConsumerSecret parameter."
        , oauthSignatureMethod = HMACSHA1
        , oauthCallback = Nothing
        }

twitterCredential :: Credential
twitterCredential =
    Credential
        [ ("oauth_token", error "You MUST specify oauth_token")
        , ("oauth_token_secret", error "You MUST specify oauth_token_secret")
        ]

makeTwitterService :: FromJSON a => Manager -> Service IO Request (Response (Either JSONError a))
makeTwitterService mgr =
    fromJSONResponseFilter .
    oauth1RequestFilter twitterOAuth twitterCredential $
    httpClientService mgr

main :: IO ()
main = do
    mgr <- newManager tlsManagerSettings
    let service = makeTwitterService mgr
    req <-
        parseRequest "https://api.twitter.com/1.1/statuses/home_timeline.json"
    runService service req $ \res ->
        case responseBody res of
            Left err -> print (err)
            Right (statuses :: [Status]) ->
                mapM_
                    (\status -> do
                         T.putStrLn $
                             T.concat
                                 [ status ^. user . screen_name
                                 , ": "
                                 , status ^. text
                                 ]
                         T.putStrLn "----")
                    statuses

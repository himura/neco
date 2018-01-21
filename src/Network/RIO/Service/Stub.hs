module Network.RIO.Service.Stub
    ( stubService
    , makeDummyResponse
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import Network.RIO.Types

stubService :: Response a -> Service m Request (Response a)
stubService res = Service $ \_req respond -> respond res

makeDummyResponse :: body -> Response body
makeDummyResponse body =
    Response
    { responseStatus = HTTP.status200
    , responseVersion = HTTP.HttpVersion 1 1
    , responseHeaders = []
    , responseCookieJar = CJ []
    , responseBody = body
    , responseClose' = ResponseClose $ return ()
    }

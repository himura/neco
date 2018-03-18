module Network.Neco.Service.Stub
    ( stubService
    , makeDummyServiceFromBodyChunks
    , makeDummyBodyReader
    , makeDummyResponse
    ) where

import qualified Data.ByteString as S
import Data.IORef
import Network.HTTP.Client
import Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import Network.Neco.Types

stubService :: Response a -> Service m Request (Response a)
stubService res = Service $ \_req respond -> respond res

makeDummyServiceFromBodyChunks :: [S.ByteString] -> Service IO Request (Response BodyReader)
makeDummyServiceFromBodyChunks bodyChunks = Service $ \_req respond -> do
    bodyReader <- makeDummyBodyReader bodyChunks
    respond $ makeDummyResponse bodyReader

makeDummyBodyReader :: [S.ByteString] -> IO BodyReader
makeDummyBodyReader bodyChunks = do
    rest <- newIORef (bodyChunks ++ [S.empty])
    return $ do
        atomicModifyIORef' rest $ \chunks ->
            case chunks of
                (x:xs) -> (xs, x)
                [] -> error "dummyBodyReader: The consumer of BodyReader eats too much"

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

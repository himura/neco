module Network.Neco.Filter.Response.ResponseCheck
    ( responseCheckFilter
    , responseStatusCheckFilter
    , HTTPStatusException(..)
    , responseStatusChecker
    ) where

import Control.Exception
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client
import qualified Network.HTTP.Types as HTTPTypes
import Network.Neco.Exceptions
import Network.Neco.Filter.Response.ByteString
import Network.Neco.Types

responseCheckFilter ::
       (Request -> Response BodyReader -> IO ()) -- ^ Response checker. The subsequent process will be unexpected result if you consume @BodyReader@ in this function.
    -> Filter IO Request Request (Response BodyReader) (Response BodyReader)
responseCheckFilter checker (Service service) =
    Service $ \req respond ->
        service req $ \res -> do
            checker req res
            respond res

responseStatusCheckFilter :: Filter IO Request Request (Response BodyReader) (Response BodyReader)
responseStatusCheckFilter = responseCheckFilter responseStatusChecker

responseStatusChecker :: Request -> Response BodyReader -> IO ()
responseStatusChecker req res
    | HTTPTypes.Status st _ <- responseStatus res
    , st >= 200 && st < 300 = return ()
    | otherwise = do
        bsChunk <- consumeBodyReader $ responseBody res
        throwIO $
            HTTPStatusException
                (responseStatus res)
                (responseHeaders res)
                (L.fromChunks bsChunk)
                req

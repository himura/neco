module Network.Neco.Filter.Response.JSON
    ( JSONError (..)
    , jsonResponseFilter
    , jsonResponseThrowFilter
    , fromJSONResponseFilter
    , fromJSONResponseThrowFilter
    , fromJSONResponseFilter'
    ) where

import Control.Exception
import Data.Aeson
import Network.HTTP.Client
import Network.Neco.Exceptions
import Network.Neco.Filter.Response.Attoparsec
import Network.Neco.Internal
import Network.Neco.Types

jsonResponseFilter ::
       Filter IO i i (Response BodyReader) (Response (Either String Value))
jsonResponseFilter = parserResponseFilter json

jsonResponseThrowFilter ::
       Filter IO i i (Response BodyReader) (Response Value)
jsonResponseThrowFilter = eitherLeftToThrowFilter JSONParseError . jsonResponseFilter

fromJSONResponseFilter ::
       FromJSON a
    => Filter IO i i (Response BodyReader) (Response (Either JSONError a))
fromJSONResponseFilter = fmapResponseFilter fromJSONResponseFilter' . jsonResponseFilter

fromJSONResponseThrowFilter ::
       FromJSON a
    => Filter IO i i (Response BodyReader) (Response a)
fromJSONResponseThrowFilter = eitherLeftToThrowFilter id . fromJSONResponseFilter

fromJSONResponseFilter' ::
       FromJSON a
    => Filter m i i (Either String Value) (Either JSONError a)
fromJSONResponseFilter' = makeResponseFilter fromJSONEither

fromJSONEither :: FromJSON a => Either String Value -> Either JSONError a
fromJSONEither (Left err) = Left $ JSONParseError err
fromJSONEither (Right value) =
    case fromJSON value of
        Success body -> Right body
        Error err -> Left $ FromJSONError err value

eitherLeftToThrowFilter :: Exception exc => (err -> exc) -> Filter IO i i (Response (Either err b)) (Response b)
eitherLeftToThrowFilter toExc =
    makeResponseFilterM $ \res ->
        case responseBody res of
            Left err -> throwIO $ toExc err
            Right body -> return $ res {responseBody = body}

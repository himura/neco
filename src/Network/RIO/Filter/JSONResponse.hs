module Network.RIO.Filter.JSONResponse
    ( jsonResponseFilter
    ) where

import Data.Aeson
import Data.Attoparsec.ByteString
import Network.RIO

jsonResponseFilter ::
       Filter IO i i (Response BodyReader) (Response (Either String Value)) r
jsonResponseFilter = makeResponseFilter $ mapM parseJSONFromBodyReader

parseJSONFromBodyReader :: BodyReader -> IO (Either String Value)
parseJSONFromBodyReader reader = eitherResult <$> parseWith reader json mempty

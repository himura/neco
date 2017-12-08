module Network.RIO.Filter.Response.Attoparsec
    ( parserResponseFilter
    ) where

import Data.Attoparsec.ByteString
import Network.HTTP.Client
import Network.RIO.Types

parseFromBodyReader :: Parser a -> BodyReader -> IO (Either String a)
parseFromBodyReader parser reader = eitherResult <$> parseWith reader parser mempty

parserResponseFilter ::
       Parser a
    -> Filter IO i i (Response BodyReader) (Response (Either String a)) r
parserResponseFilter parser = makeResponseFilter $ mapM (parseFromBodyReader parser)

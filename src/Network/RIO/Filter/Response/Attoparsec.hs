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
    -> Filter i i (Response BodyReader) (Response (Either String a)) (IO r)
parserResponseFilter parser = makeResponseFilterM $ mapM (parseFromBodyReader parser)

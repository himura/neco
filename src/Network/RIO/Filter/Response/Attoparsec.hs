module Network.RIO.Filter.Response.Attoparsec
    ( parserResponseFilter
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.ByteString as S
import Network.HTTP.Client
import Network.RIO.Types
import Prelude -- to suppress redundant import warning about Control.Applicative

parseFromBodyReader :: Parser a -> BodyReader -> IO (Either String a)
parseFromBodyReader parser reader = eitherResult <$> parseWith reader parser S.empty

parserResponseFilter ::
       Parser a
    -> Filter i i (Response BodyReader) (Response (Either String a)) (IO r)
parserResponseFilter parser = makeResponseFilterM $ mapM (parseFromBodyReader parser)

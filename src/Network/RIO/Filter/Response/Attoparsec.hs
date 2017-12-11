module Network.RIO.Filter.Response.Attoparsec
    ( parserResponseFilter
    ) where

import Control.Applicative
import Data.Attoparsec.ByteString
import qualified Data.ByteString as S
import Network.HTTP.Client
import Network.RIO.Types

-- for GHC 7.8
import Data.Traversable
import Prelude hiding (mapM)

parseFromBodyReader :: Parser a -> BodyReader -> IO (Either String a)
parseFromBodyReader parser reader = eitherResult <$> parseWith reader parser S.empty

parserResponseFilter ::
       Parser a
    -> Filter i i (Response BodyReader) (Response (Either String a)) (IO r)
parserResponseFilter parser = makeResponseFilterM $ mapM (parseFromBodyReader parser)

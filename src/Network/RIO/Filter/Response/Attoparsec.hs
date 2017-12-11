module Network.RIO.Filter.Response.Attoparsec
    ( parserResponseFilter
    ) where

import Data.Attoparsec.ByteString
import qualified Data.ByteString as S
import Network.HTTP.Client
import Network.RIO.Types

-- for GHC 7.8
import Control.Applicative
import Data.Traversable
import Prelude hiding (mapM)

parseFromBodyReader :: Parser a -> BodyReader -> IO (Either String a)
parseFromBodyReader parser reader = eitherResult <$> parseWith reader parser S.empty

parserResponseFilter ::
       Parser a
    -> Filter (IO r) i i (Response BodyReader) (Response (Either String a))
parserResponseFilter parser = makeResponseFilterM $ mapM (parseFromBodyReader parser)

module Network.Neco.Exceptions where

import Control.Exception
import qualified Network.HTTP.Types as HTTPTypes
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client (Request)
import Data.Aeson (Value)

data HTTPStatusException =
    HTTPStatusException
        { errorResponseStatusCode :: HTTPTypes.Status
        , errorResponseHeaders :: [HTTPTypes.Header]
        , errorResponseBody :: L.ByteString
        , errorRequest :: Request
        }
    deriving (Show)
instance Exception HTTPStatusException

data JSONError
    = JSONParseError String
    | FromJSONError String Value
    deriving (Eq, Show)
instance Exception JSONError


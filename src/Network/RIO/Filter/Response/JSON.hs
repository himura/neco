module Network.RIO.Filter.Response.JSON
    ( jsonResponseFilter
    ) where

import Data.Aeson
import Network.HTTP.Client
import Network.RIO.Filter.Response.Attoparsec
import Network.RIO.Types

jsonResponseFilter ::
       Filter IO i i (Response BodyReader) (Response (Either String Value)) r
jsonResponseFilter = parserResponseFilter json

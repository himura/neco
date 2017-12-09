module Network.RIO.Service.Stub
    ( stubService
    ) where

import Network.HTTP.Client
import Network.RIO.Types

stubService :: Response a -> Service Request (Response a) r
stubService res _req respond = respond res

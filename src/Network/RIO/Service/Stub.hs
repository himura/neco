module Network.RIO.Service.Stub
    ( stubService
    ) where

import Network.HTTP.Client
import Network.RIO.Types

stubService :: Response a -> Service m Request (Response a)
stubService res = Service $ \_req respond -> respond res

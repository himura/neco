module Network.Neco.Service.HttpClient
    ( httpClientService
    ) where

import Network.HTTP.Client
import Network.Neco.Types

httpClientService :: Manager -> Service IO Request (Response BodyReader)
httpClientService mgr = Service $ \req respond -> withResponse req mgr respond

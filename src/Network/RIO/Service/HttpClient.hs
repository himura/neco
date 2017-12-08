module Network.RIO.Service.HttpClient
    ( httpClientService
    ) where

import Network.HTTP.Client
import Network.RIO.Types

httpClientService :: Manager -> Service IO Request (Response BodyReader) r
httpClientService mgr req respond = withResponse req mgr respond

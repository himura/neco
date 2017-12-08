module Network.RIO.HttpClient
    ( makeHttpClientService
    ) where

import Network.HTTP.Client
import Network.RIO.Types

makeHttpClientService ::
       Filter IO req Request (Response BodyReader) res r
    -> Manager
    -> Service IO req res r
makeHttpClientService filt mgr = filt $ flip withResponse mgr

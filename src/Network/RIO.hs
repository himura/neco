module Network.RIO
    ( Service
    , Filter
    , SimpleFilter
    , makeFilter
    , makeRequestFilter
    , makeResponseFilter

    , makeHttpClientService

    -- * re-exports
    , module Network.HTTP.Client
    , tlsManagerSettings
    ) where

import Network.RIO.Types
import Network.RIO.HttpClient
import Network.HTTP.Client
import Network.HTTP.Client.TLS

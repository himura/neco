module Network.RIO
    ( Service (..)
    , Filter
    , makeFilter
    , makeFilterM
    , makeRequestFilter
    , makeRequestFilterM
    , makeResponseFilter
    , makeResponseFilterM

    , httpClientService

    , module Network.RIO.Filter.Response

    -- * re-exports
    , module Network.HTTP.Client
    , tlsManagerSettings
    ) where

import Network.RIO.Types
import Network.RIO.Service.HttpClient
import Network.RIO.Filter.Response
import Network.HTTP.Client
import Network.HTTP.Client.TLS

module Network.Neco
    ( Service (..)
    , Filter
    , makeFilter
    , makeFilterM
    , makeRequestFilter
    , makeRequestFilterM
    , makeResponseFilter
    , makeResponseFilterM

    , httpClientService

    , module Network.Neco.Filter.Response

    -- * re-exports
    , module Network.HTTP.Client
    , tlsManagerSettings
    ) where

import Network.Neco.Types
import Network.Neco.Service.HttpClient
import Network.Neco.Filter.Response
import Network.HTTP.Client
import Network.HTTP.Client.TLS

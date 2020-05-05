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

    , module Network.Neco.Exceptions
    , module Network.Neco.Filter.Response

    -- * re-exports
    , module Network.HTTP.Client
    , tlsManagerSettings
    ) where

import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.Neco.Exceptions
import Network.Neco.Filter.Response
import Network.Neco.Internal
import Network.Neco.Service.HttpClient
import Network.Neco.Types

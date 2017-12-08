module Network.RIO.Filter.Request.OAuth1
    ( oauth1RequestFilter
    ) where

import Network.HTTP.Client
import Network.RIO.Types
import Web.Authenticate.OAuth

oauth1RequestFilter :: OAuth -> Credential -> SimpleFilter IO Request res r
oauth1RequestFilter oauth credential =
    makeRequestFilter $ signOAuth oauth credential

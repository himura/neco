module Network.RIO.Filter.Request.OAuth1
    ( oauth1RequestFilter
    ) where

import Network.HTTP.Client
import Network.RIO.Types
import Web.Authenticate.OAuth

oauth1RequestFilter :: OAuth -> Credential -> Filter Request Request res res (IO r)
oauth1RequestFilter oauth credential =
    makeRequestFilterM $ signOAuth oauth credential

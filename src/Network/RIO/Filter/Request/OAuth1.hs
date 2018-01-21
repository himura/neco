module Network.RIO.Filter.Request.OAuth1
    ( oauth1RequestFilter
    -- * re-exports
    , module Web.Authenticate.OAuth
    ) where

import Network.HTTP.Client
import Network.RIO.Types
import Web.Authenticate.OAuth

oauth1RequestFilter :: OAuth -> Credential -> Filter IO Request Request res res
oauth1RequestFilter oauth credential =
    makeRequestFilterM $ signOAuth oauth credential

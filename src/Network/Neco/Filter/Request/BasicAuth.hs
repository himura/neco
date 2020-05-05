module Network.Neco.Filter.Request.BasicAuth
    ( basicAuthRequestFilter
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.Neco.Internal
import Network.Neco.Types

basicAuthRequestFilter ::
       ByteString -- ^ username
    -> ByteString -- ^ password
    -> Filter m Request Request res res
basicAuthRequestFilter user pass =
    makeRequestFilter $ applyBasicAuth user pass

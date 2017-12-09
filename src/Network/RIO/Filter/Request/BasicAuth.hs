module Network.RIO.Filter.Request.BasicAuth
    ( basicAuthRequestFilter
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.RIO.Types

basicAuthRequestFilter ::
       ByteString -- ^ username
    -> ByteString -- ^ password
    -> Filter Request Request res res r
basicAuthRequestFilter user pass =
    makeRequestFilter $ applyBasicAuth user pass

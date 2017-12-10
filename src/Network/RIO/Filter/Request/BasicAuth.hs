module Network.RIO.Filter.Request.BasicAuth
    ( basicAuthRequestFilter
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.RIO.Types

basicAuthRequestFilter ::
       ByteString -- ^ username
    -> ByteString -- ^ password
    -> Filter r Request Request res res
basicAuthRequestFilter user pass =
    makeRequestFilter $ applyBasicAuth user pass

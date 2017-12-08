module Network.RIO.Filter.Request.BasicAuth
    ( basicAuthRequestFilter
    ) where

import Data.ByteString (ByteString)
import Network.HTTP.Client
import Network.RIO.Types

basicAuthRequestFilter ::
       Monad m
    => ByteString -- ^ username
    -> ByteString -- ^ password
    -> Filter m Request Request res res r
basicAuthRequestFilter user pass =
    makeRequestFilter $ return . applyBasicAuth user pass

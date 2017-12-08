module Network.RIO.Types where

import Control.Monad
import Network.HTTP.Client

type Service m req res r = req -> (res -> m r) -> m r
type Filter m reqIn reqOut resOut resIn r
     = Service m reqOut resOut r -> Service m reqIn resIn r
type SimpleFilter m req res r = Filter m req req res res r

makeFilter ::
       Monad m
    => (reqIn -> m reqOut)
    -> (resOut -> m resIn)
    -> Filter m reqIn reqOut resOut resIn r
makeFilter requestFilter responseFilter service req respond =
    requestFilter req >>= flip service (responseFilter >=> respond)

makeRequestFilter ::
       Monad m => (reqIn -> m reqOut) -> Filter m reqIn reqOut res res r
makeRequestFilter f = makeFilter f return

makeResponseFilter ::
       Monad m => (resOut -> m resIn) -> Filter m req req resOut resIn r
makeResponseFilter = makeFilter return

toService ::
       Filter IO req Request (Response BodyReader) res r
    -> Manager
    -> Service IO req res r
toService filt mgr = filt $ flip withResponse mgr

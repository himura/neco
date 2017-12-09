module Network.RIO.Types
    ( Service
    , Filter
    , makeFilter
    , makeFilterM
    , makeRequestFilter
    , makeRequestFilterM
    , makeResponseFilter
    , makeResponseFilterM

    , unwrap
    , unwrapRequestFilter
    , unwrapResponseFilter

    , fmapRequestFilter
    , fmapResponseFilter
    ) where

import Control.Monad

type Service req res r = req -> (res -> r) -> r
type Filter reqIn reqOut resOut resIn r
     = Service reqOut resOut r -> Service reqIn resIn r

makeFilter ::
       (reqIn -> reqOut)
    -> (resOut -> resIn)
    -> Filter reqIn reqOut resOut resIn r
makeFilter requestFilter responseFilter service req respond =
    service (requestFilter req) $ respond . responseFilter

makeFilterM ::
       Monad m
    => (reqIn -> m reqOut)
    -> (resOut -> m resIn)
    -> Filter reqIn reqOut resOut resIn (m r)
makeFilterM requestFilter responseFilter service req respond =
    requestFilter req >>= flip service (responseFilter >=> respond)

makeRequestFilter :: (reqIn -> reqOut) -> Filter reqIn reqOut res res r
makeRequestFilter f service req = service (f req)

makeRequestFilterM ::
       Monad m => (reqIn -> m reqOut) -> Filter reqIn reqOut res res (m r)
makeRequestFilterM f service req respond = f req >>= flip service respond

makeResponseFilter :: (resOut -> resIn) -> Filter req req resOut resIn r
makeResponseFilter f service req respond = service req $ respond . f

makeResponseFilterM ::
       Monad m => (resOut -> m resIn) -> Filter req req resOut resIn (m r)
makeResponseFilterM f service req respond = service req $ f >=> respond

unwrap :: Filter a i i b b -> a -> b
unwrap filt a = filt (flip id) a id

unwrapRequestFilter :: Filter reqIn reqOut reqOut reqOut reqOut -> reqIn -> reqOut
unwrapRequestFilter = unwrap

unwrapResponseFilter :: Filter resOut resOut resOut resIn resIn -> resOut -> resIn
unwrapResponseFilter = unwrap

fmapRequestFilter :: Functor f => Filter reqIn reqOut reqOut reqOut reqOut -> Filter (f reqIn) (f reqOut) req req r
fmapRequestFilter filt = makeRequestFilter $ fmap (unwrapRequestFilter filt)

fmapResponseFilter :: Functor f => Filter resOut resOut resOut resIn resIn -> Filter req req (f resOut) (f resIn) r
fmapResponseFilter filt = makeResponseFilter $ fmap (unwrapResponseFilter filt)

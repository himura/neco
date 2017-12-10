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

type Service r req res = req -> (res -> r) -> r
type Filter r reqIn reqOut resOut resIn
     = Service r reqOut resOut -> Service r reqIn resIn

makeFilter ::
       (reqIn -> reqOut)
    -> (resOut -> resIn)
    -> Filter r reqIn reqOut resOut resIn
makeFilter requestFilter responseFilter service req respond =
    service (requestFilter req) $ respond . responseFilter

makeFilterM ::
       Monad m
    => (reqIn -> m reqOut)
    -> (resOut -> m resIn)
    -> Filter (m r) reqIn reqOut resOut resIn
makeFilterM requestFilter responseFilter service req respond =
    requestFilter req >>= flip service (responseFilter >=> respond)

makeRequestFilter :: (reqIn -> reqOut) -> Filter r reqIn reqOut res res
makeRequestFilter f service req = service (f req)

makeRequestFilterM ::
       Monad m => (reqIn -> m reqOut) -> Filter (m r) reqIn reqOut res res
makeRequestFilterM f service req respond = f req >>= flip service respond

makeResponseFilter :: (resOut -> resIn) -> Filter r req req resOut resIn
makeResponseFilter f service req respond = service req $ respond . f

makeResponseFilterM ::
       Monad m => (resOut -> m resIn) -> Filter (m r) req req resOut resIn
makeResponseFilterM f service req respond = service req $ f >=> respond

unwrap :: Filter b a i i b -> a -> b
unwrap filt a = filt (flip id) a id

unwrapRequestFilter :: Filter reqOut reqIn reqOut reqOut reqOut -> reqIn -> reqOut
unwrapRequestFilter = unwrap

unwrapResponseFilter :: Filter resIn resOut resOut resOut resIn -> resOut -> resIn
unwrapResponseFilter = unwrap

fmapRequestFilter :: Functor f => Filter reqOut reqIn reqOut reqOut reqOut -> Filter r (f reqIn) (f reqOut) req req
fmapRequestFilter filt = makeRequestFilter $ fmap (unwrapRequestFilter filt)

fmapResponseFilter :: Functor f => Filter resIn resOut resOut resOut resIn -> Filter r req req (f resOut) (f resIn)
fmapResponseFilter filt = makeResponseFilter $ fmap (unwrapResponseFilter filt)

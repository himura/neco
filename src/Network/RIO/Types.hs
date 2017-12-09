module Network.RIO.Types
    ( Service
    , Filter
    , makeFilter
    , makeFilterM
    , makeRequestFilter
    , makeRequestFilterM
    , makeResponseFilter
    , makeResponseFilterM
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
makeRequestFilter f = makeFilter f id

makeRequestFilterM ::
       Monad m => (reqIn -> m reqOut) -> Filter reqIn reqOut res res (m r)
makeRequestFilterM f = makeFilterM f return

makeResponseFilter :: (resOut -> resIn) -> Filter req req resOut resIn r
makeResponseFilter = makeFilter id

makeResponseFilterM ::
       Monad m => (resOut -> m resIn) -> Filter req req resOut resIn (m r)
makeResponseFilterM = makeFilterM return

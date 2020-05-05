{-# LANGUAGE RankNTypes #-}

module Network.Neco.Types
    ( Service (..)
    , Filter
    ) where

import Data.Profunctor.Unsafe

newtype Service m req res = Service { runService :: forall r. req -> (res -> m r) -> m r }
type Filter m reqIn reqOut resOut resIn
    = Service m reqOut resOut -> Service m reqIn resIn

instance Profunctor (Service m) where
    dimap f g (Service service) = Service $ \req respond -> service (f req) (respond . g)
    {-# INLINE dimap #-}

    lmap f (Service service) = Service $ \req respond -> service (f req) respond
    {-# INLINE lmap #-}

    rmap g (Service service) = Service $ \req respond -> service req (respond . g)
    {-# INLINE rmap #-}

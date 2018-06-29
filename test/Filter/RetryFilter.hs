{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter.RetryFilter where

import Data.IORef
import Network.Neco
import Network.Neco.Filter.RetryFilter
import System.Random
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

data Request =
    Request
    deriving (Show, Eq)

data Response = Response
    { status :: Int
    } deriving (Show, Eq)

noWaitBackoff :: gen -> RetryBackoff
noWaitBackoff = const (repeat 0)

case_retryFilter_withMaxAttempt :: Assertion
case_retryFilter_withMaxAttempt = do
    let tries = 10
        shouldRetry res = status res /= 200
        retryPolicy = maxAttemptRetryPolicy tries shouldRetry noWaitBackoff
    counterRef <- newIORef (0 :: Int)
    let underlayingService = Service $ \_req respond -> do
            () <- atomicModifyIORef counterRef (\n -> (n + 1, ()))
            respond $ Response 500
        service = retryFilter retryPolicy underlayingService

    res <- runService service Request return

    res @?= Response 500
    readIORef counterRef >>= \c -> c @?= 10

case_retryFilter_withMaxAttempt_endIfSucceeded :: Assertion
case_retryFilter_withMaxAttempt_endIfSucceeded = do
    let tries = 10
        shouldRetry res = status res /= 200
        retryPolicy = maxAttemptRetryPolicy tries shouldRetry noWaitBackoff
    counterRef <- newIORef (0 :: Int)
    let underlayingService = Service $ \_req respond -> do
            n <- atomicModifyIORef counterRef (\n -> (n + 1, n))
            respond $ Response $ if n == 5 then 200 else 500
        service = retryFilter retryPolicy underlayingService

    res <- runService service Request return

    res @?= Response 200
    readIORef counterRef >>= \c -> c @?= 6

case_jitteredBackoff :: Assertion
case_jitteredBackoff = do
    gen <- newStdGen
    let base = 100*1000
        limit = 10*1000*1000
        backoff = jitteredBackoff base limit gen
        waits = take 100 backoff

    length waits @?= 100
    filter (< base) waits @?= []
    filter (> limit) waits @?= []

tests :: TestTree
tests = $(testGroupGenerator)

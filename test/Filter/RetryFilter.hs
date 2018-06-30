{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter.RetryFilter where

import Control.Exception
import Data.IORef
import Network.Neco.Filter.RetryFilter
import Network.Neco.Types
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

data HttpException
    = HttpRetryableException String
    | HttpFatalException String
    deriving (Show, Eq)
instance Exception HttpException

noWaitBackoff :: gen -> RetryBackoff
noWaitBackoff = const (repeat 0)

noWaitRetryPolicy :: RetryPolicy IO Response
noWaitRetryPolicy = maxAttemptRetryPolicy tries shouldRetry noWaitBackoff
  where
    tries = 10
    shouldRetry (Right res) = status res /= 200
    shouldRetry (Left exc)
        | Just httpException <- fromException exc =
            case httpException of
                HttpRetryableException _ -> True
                _ -> False
        | otherwise = False

case_retryFilter_withMaxAttempt_retriesSpecifiedCount_withErrorResponse :: Assertion
case_retryFilter_withMaxAttempt_retriesSpecifiedCount_withErrorResponse = do
    counterRef <- newIORef (0 :: Int)
    let underlayingService = Service $ \_req respond -> do
            () <- atomicModifyIORef counterRef (\n -> (n + 1, ()))
            respond $ Response 500
        service = retryFilter noWaitRetryPolicy underlayingService

    res <- runService service Request return

    res @?= Response 500
    readIORef counterRef >>= \c -> c @?= 10

case_retryFilter_withMaxAttempt_endIfSucceeded :: Assertion
case_retryFilter_withMaxAttempt_endIfSucceeded = do
    counterRef <- newIORef (0 :: Int)
    let underlayingService = Service $ \_req respond -> do
            n <- atomicModifyIORef counterRef (\n -> (n + 1, n))
            respond $ Response $ if n == 5 then 200 else 500
        service = retryFilter noWaitRetryPolicy underlayingService

    res <- runService service Request return

    res @?= Response 200
    readIORef counterRef >>= \c -> c @?= 6

case_retryFilter_withMaxAttempt_endIfSucceeded_withRetryableException :: Assertion
case_retryFilter_withMaxAttempt_endIfSucceeded_withRetryableException = do
    counterRef <- newIORef (0 :: Int)
    let underlayingService = Service $ \_req respond -> do
            n <- atomicModifyIORef counterRef (\n -> (n + 1, n))
            if n == 5
                then respond $ Response 200
                else throwIO $ HttpRetryableException "some reason"
        service = retryFilter noWaitRetryPolicy underlayingService

    res <- runService service Request return

    res @?= Response 200
    readIORef counterRef >>= \c -> c @?= 6

case_retryFilter_withMaxAttempt_throwExceptionIfNonRetryableException :: Assertion
case_retryFilter_withMaxAttempt_throwExceptionIfNonRetryableException = do
    counterRef <- newIORef (0 :: Int)
    let underlayingService = Service $ \_req _respond -> do
            () <- atomicModifyIORef counterRef (\n -> (n + 1, ()))
            throwIO $ HttpFatalException "fatal"
        service = retryFilter noWaitRetryPolicy underlayingService

    resOrException <- try $ runService service Request return

    resOrException @?= Left (HttpFatalException "fatal")
    readIORef counterRef >>= \c -> c @?= 1

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

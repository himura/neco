module Network.Neco.Filter.RetryFilter where

import Control.Concurrent
import Control.Exception
import Network.Neco.Types
import System.Random

newtype RetryPolicy m res = RetryPolicy
    { generateRetryHandler :: m (RetryHandler res)
    }

newtype RetryHandler res = RetryHandler
    { applyRetryPolicy :: Either SomeException res -> Maybe (Int, RetryHandler res)
    }

type RetryBackoff = [Int]

-- | decorrelated jittered backoff
jitteredBackoff ::
       RandomGen gen
    => Int -- ^ base
    -> Int -- ^ cap
    -> gen
    -> RetryBackoff
jitteredBackoff base cap = go base
  where
    go prev gen =
        let (curUncap, nextGen) = randomR (base, prev * 3) gen
            cur = min cap curUncap
        in cur : go cur nextGen

maxAttemptRetryHandler ::
       Int -- ^ max attempts
    -> (Either SomeException res -> Bool) -- ^ response classifier. if it True then retry.
    -> RetryBackoff
    -> RetryHandler res
maxAttemptRetryHandler maxAttempts shouldRetry bareBackoff =
    RetryHandler $ go backoff
  where
    backoff = take (maxAttempts - 1) bareBackoff
    go [] _ = Nothing
    go (b:bs) res
        | shouldRetry res = Just (b, RetryHandler (go bs))
        | otherwise = Nothing

maxAttemptRetryPolicy ::
       Int -- ^ max attempts
    -> (Either SomeException res -> Bool) -- ^ response classifier. if it True then retry.
    -> (StdGen -> RetryBackoff)
    -> RetryPolicy IO res
maxAttemptRetryPolicy maxAttempts shouldRetry backoffGenerator =
    RetryPolicy $ do
        gen <- newStdGen
        return $
            maxAttemptRetryHandler
                maxAttempts
                shouldRetry
                (backoffGenerator gen)

retryFilter ::
       RetryPolicy IO res
    -> Filter IO req req res res
retryFilter retryPolicy service =
    Service $ \req respond -> do
        retryHandler <- generateRetryHandler retryPolicy
        go retryHandler req respond
  where
    go handler req respond = do
        resOrException <-
            try $
            runService service req $ \res -> do
                case applyRetryPolicy handler (Right res) of
                    Just (wait, nextHandler) ->
                        return $ Left (wait, nextHandler)
                    Nothing -> Right <$> respond res
        resOrRetry <-
            case resOrException of
                Left exc ->
                    case applyRetryPolicy handler (Left exc) of
                        Just (wait, nextHandler) ->
                            return $ Left (wait, nextHandler)
                        Nothing -> throwIO exc
                Right r -> return r
        case resOrRetry of
            Left (wait, nextHandler) -> do
                threadDelay wait
                go nextHandler req respond
            Right r -> return r

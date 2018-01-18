{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter.Request where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as S8
import Network.RIO
import Network.RIO.Filter.Request.BasicAuth
import Network.RIO.Filter.Request.OAuth2
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_basicAuthRequestFilter :: Assertion
case_basicAuthRequestFilter = do
    let requestFilter = basicAuthRequestFilter "rio" "mikoshiba"
        service = requestFilter $ Service $ \req respond -> do
            requestHeaders req @?= [("Authorization", S8.concat ["Basic ", Base64.encode "rio:mikoshiba"])]
            respond ()
    req <- parseRequest "http://localhost:18080/"
    runService service req return

case_oauth2RequestFilter :: Assertion
case_oauth2RequestFilter = do
    let accessToken = AccessToken "TestAccessToken" 3600 "RefleshToken" "Bearer"
    let requestFilter = oauth2RequestFilter accessToken
        service = requestFilter $ Service $ \req respond -> do
            requestHeaders req @?= [("Authorization", S8.concat ["Bearer TestAccessToken"])]
            respond ()
    req <- parseRequest "http://localhost:18080/"
    runService service req return

tests :: TestTree
tests = $(testGroupGenerator)

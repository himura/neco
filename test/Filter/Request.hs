{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter.Request where

import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as S8
import Network.RIO
import Network.RIO.Filter.Request.BasicAuth
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

case_basicAuthRequestFilter :: Assertion
case_basicAuthRequestFilter = do
    let requestFilter = basicAuthRequestFilter "rio" "mikoshiba"
        service = requestFilter $ \req respond -> do
            requestHeaders req @?= [("Authorization", S8.concat ["Basic ", Base64.encode "rio:mikoshiba"])]
            respond ()
    req <- parseRequest "http://localhost:18080/"
    service req return

tests :: TestTree
tests = $(testGroupGenerator)

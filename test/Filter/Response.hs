{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Filter.Response where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.IORef
import qualified Data.Text as T
import GHC.Generics
import Network.HTTP.Client.Internal
import qualified Network.HTTP.Types as HTTP
import Network.RIO
import Network.RIO.Filter.Response.Attoparsec
import Network.RIO.Filter.Response.ByteString
import Network.RIO.Filter.Response.JSON
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.TH

body :: [S8.ByteString]
body = ["Mikoshiba Rio\n", "Mikoshiba Rea\n", "Clover Heart's\n"]

bodyJson :: [S8.ByteString]
bodyJson =
    [ "{ \"characters\": [{\"name\": \"Mikoshiba Rio\", \"birthday\": \"24 April\"},{\"name\": \"Mikoshiba Rea\", \"birthday\": \"24 April\"}] }"
    ]

bodyJsonPartial :: [S8.ByteString]
bodyJsonPartial =
    [ "{ \"characters\": [{\"name\": \"Mikoshiba Rio\", \"birthday\": \"24 April\"}"
    , ",{\"name\": \"Mikoshiba Rea\", \"birthday\": \"24 April\"}] }"
    ]

data CharacterList = CharacterList
    { characters :: [Character]
    } deriving (Eq, Show, Read, Generic)
instance FromJSON CharacterList where
    parseJSON = genericParseJSON defaultOptions

data Character = Character
    { name :: T.Text
    , birthday :: T.Text
    } deriving (Eq, Show, Read, Generic)
instance FromJSON Character where
    parseJSON = genericParseJSON defaultOptions

dummyBodyReader :: [S8.ByteString] -> IO BodyReader
dummyBodyReader bodyChunks = do
    rest <- newIORef (bodyChunks ++ [""])
    return $ do
        atomicModifyIORef' rest $ \chunks ->
            case chunks of
                (x:xs) -> (xs, x)
                [] -> error "dummyBodyReader: The consumer of BodyReader eats too much"

dummyService :: [S8.ByteString] -> Service IO req (Response BodyReader) r
dummyService bodyChunks _req respond = do
    bodyReader <- dummyBodyReader bodyChunks
    respond $
        Response
        { responseStatus = HTTP.status200
        , responseVersion = HTTP.HttpVersion 1 1
        , responseHeaders = []
        , responseCookieJar = CJ []
        , responseBody = bodyReader
        , responseClose' = ResponseClose $ return ()
        }

case_bsChunksResponseFilter :: Assertion
case_bsChunksResponseFilter = do
    let service = bsChunksResponseFilter $ dummyService body
    req <- parseRequest "http://localhost:18080"
    service req $ \res ->
        responseBody res @?= body

case_lbsResponseFilter :: Assertion
case_lbsResponseFilter = do
    let service = lbsResponseFilter $ dummyService body
    req <- parseRequest "http://localhost:18080"
    service req $ \res ->
        responseBody res @?= L8.fromChunks body

jsonResponseFilterExample :: Service IO Request (Response (Either String Value)) () -> Assertion
jsonResponseFilterExample service = do
    req <- parseRequest "http://localhost:18080"
    service req $ \res ->
        responseBody res ^.. _Right . key "characters" . _Array . traversed . key "name" . _String @?= ["Mikoshiba Rio", "Mikoshiba Rea"]

case_jsonResponseFilter :: Assertion
case_jsonResponseFilter = do
    let service = jsonResponseFilter $ dummyService bodyJson
    jsonResponseFilterExample service

case_jsonResponseFilterWithPartial :: Assertion
case_jsonResponseFilterWithPartial = do
    let service = jsonResponseFilter $ dummyService bodyJsonPartial
    jsonResponseFilterExample service

fromJSONResponseFilterExample :: Service IO Request (Response (Either JSONError CharacterList)) () -> Assertion
fromJSONResponseFilterExample service = do
    req <- parseRequest "http://localhost:18080"
    service req $ \res ->
        responseBody res @?=
        Right
            (CharacterList
                 [ Character "Mikoshiba Rio" "24 April"
                 , Character "Mikoshiba Rea" "24 April"
                 ])

case_fromJSONResponseFilter :: Assertion
case_fromJSONResponseFilter = do
    let service = fromJSONResponseFilter $ dummyService bodyJson
    fromJSONResponseFilterExample service

case_fromJSONResponseFilterWithPartial :: Assertion
case_fromJSONResponseFilterWithPartial = do
    let service = fromJSONResponseFilter $ dummyService bodyJsonPartial
    fromJSONResponseFilterExample service

tests :: TestTree
tests = $(testGroupGenerator)

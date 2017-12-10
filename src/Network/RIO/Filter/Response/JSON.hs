{-# LANGUAGE DeriveDataTypeable #-}

module Network.RIO.Filter.Response.JSON
    ( JSONError (..)
    , jsonResponseFilter
    , fromJSONResponseFilter
    , fromJSONResponseFilter'
    ) where

import Control.Exception
import Data.Aeson
import Data.Typeable
import Network.HTTP.Client
import Network.RIO.Filter.Response.Attoparsec
import Network.RIO.Types

data JSONError
    = JSONParseError String
    | FromJSONError String Value
    deriving (Eq, Show, Typeable)
instance Exception JSONError

jsonResponseFilter ::
       Filter i i (Response BodyReader) (Response (Either String Value)) (IO r)
jsonResponseFilter = parserResponseFilter json

fromJSONResponseFilter ::
       FromJSON a
    => Filter i i (Response BodyReader) (Response (Either JSONError a)) (IO r)
fromJSONResponseFilter = fmapResponseFilter fromJSONResponseFilter' . jsonResponseFilter

fromJSONResponseFilter' ::
       FromJSON a
    => Filter i i (Either String Value) (Either JSONError a) r
fromJSONResponseFilter' = makeResponseFilter fromJSONEither

fromJSONEither :: FromJSON a => Either String Value -> Either JSONError a
fromJSONEither (Left err) = Left $ JSONParseError err
fromJSONEither (Right value) =
    case fromJSON value of
        Success body -> Right body
        Error err -> Left $ FromJSONError err value

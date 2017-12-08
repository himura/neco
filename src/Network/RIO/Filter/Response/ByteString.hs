module Network.RIO.Filter.Response.ByteString
    ( lbsResponseFilter
    , bsChunksResponseFilter
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client
import Network.RIO.Types

bsChunksResponseFilter :: Filter IO i i (Response BodyReader) (Response [S.ByteString]) r
bsChunksResponseFilter = makeResponseFilter $ mapM consumeBodyReader

lbsResponseFilter :: Filter IO i i (Response BodyReader) (Response L.ByteString) r
lbsResponseFilter = makeResponseFilter $ mapM (fmap L.fromChunks . consumeBodyReader)

consumeBodyReader :: BodyReader -> IO [S.ByteString]
consumeBodyReader reader = go id
  where
    go f = do
        chunk <- reader
        if S.null chunk
            then return $ f mempty
            else go (f . (chunk :))

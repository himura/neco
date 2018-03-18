module Network.Neco.Filter.Response.ByteString
    ( lbsResponseFilter
    , bsChunksResponseFilter
    ) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Network.HTTP.Client
import Network.Neco.Types

-- for GHC 7.8
import Data.Traversable
import Prelude hiding (mapM)

bsChunksResponseFilter :: Filter IO i i (Response BodyReader) (Response [S.ByteString])
bsChunksResponseFilter = makeResponseFilterM $ mapM consumeBodyReader

lbsResponseFilter :: Filter IO i i (Response BodyReader) (Response L.ByteString)
lbsResponseFilter = makeResponseFilterM $ mapM (fmap L.fromChunks . consumeBodyReader)

consumeBodyReader :: BodyReader -> IO [S.ByteString]
consumeBodyReader reader = go id
  where
    go f = do
        chunk <- reader
        if S.null chunk
            then return $ f []
            else go (f . (chunk :))

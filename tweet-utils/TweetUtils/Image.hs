module TweetUtils.Image (Image (..), fetchImage) where

import Data.ByteString.Lazy qualified as Lazy
import Data.Binary.Builder qualified as Builder
import Web.Twitter.Conduit.Base (ResponseBodyType (..))
import Conduit (runConduit, (.|), throwM, sinkLazyBuilder, mapC)
import Network.HTTP.Types qualified as HT
import Web.Twitter.Conduit.Response 
  (Response (..), TwitterError (..))
import Data.Aeson qualified as Aeson
import Data.Text qualified as T
import Web.Twitter.Conduit.Request (APIRequest (..))
import TweetUtils.Render (ImageInfo (..))

newtype Image = Image { imgBytes :: Lazy.ByteString }

instance ResponseBodyType Image where
  parseResponseBody res =
    case responseStatus res of
      st | st == HT.status200 -> do
       out <- 
         runConduit $ responseBody res 
           .| mapC Builder.fromByteString
           .| sinkLazyBuilder
       pure $ res {responseBody = Image out}
      _ -> do
        throwM $ 
          TwitterStatusError 
            (responseStatus res) 
            (responseHeaders res) 
            Aeson.Null

fetchImage :: ImageInfo -> APIRequest '[] Image
fetchImage (ImageInfo url) = 
  APIRequest "GET" (T.unpack url) []
fetchImage (VideoInfo url []) = 
  APIRequest "GET" (T.unpack url) []
fetchImage (VideoInfo _ (url : _)) = 
  APIRequest "GET" (T.unpack url) []

  

      




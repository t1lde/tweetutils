module TweetUtils.Commands.DumpTweets
  ( apiDumpTweets
  , DumpTweetOptions (..)
  ) where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe, isJust)
import Control.Arrow
import Control.Monad
import Control.Applicative
import Data.Foldable (for_)

--------------------------------------------------------------------------------

import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath.Posix ((</>), FilePath)
import Data.ByteString.Lazy qualified as LBS
import Control.Lens hiding (children)
import Control.Monad.State.Class
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text qualified as T
import qualified Data.Text.IO as T

--------------------------------------------------------------------------------

import Conduit
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens hiding (user)
import Text.Pandoc.Writers qualified as Pandoc
import Text.Pandoc.Class qualified as Pandoc
import Data.Tree
import Control.Monad.State.Lazy (execStateT, evalStateT)

--------------------------------------------------------------------------------

import TweetUtils.Render
  ( renderTweets, defaultRenderConfig, RenderOutputs (..), processRenderT, imageInfoUrl
  , imageName
  )
import TweetUtils.Image (fetchImage, imgBytes)
import TweetUtils.Query
  (buildTweetTree, parents, collectTweet, interacted, lookupMentions, lookupSearchReplies, userTL, showUser)
import TweetUtils.MonadApp (MonadApp, Named ((:=)), logNormal, logDebug, callTwitter, config)

--------------------------------------------------------------------------------

type DumpTweetInfo =
   '[ "outDir" ':= FilePath
    , "format"':= FormatType
    , "downloadMedia" ':= Bool
    ]

data FormatType = Markdown | Html | Org
  deriving stock Show

--------------------------------------------------------------------------------

apiDumpTweets ::
  ( MonadApp DumpTweetInfo m) =>
  m ()
apiDumpTweets = do
  logNormal "Verifying access credentials..."
  user <- callTwitter accountVerifyCredentials
  putLog "Authenticated as user " <> (showUser user)

  tweets <-
    streamTwitter (userTL user)
      & fmap (foldMap collectTweet)

  let
    minId = maybe 0 fst $ M.lookupMin tweets

  replies <-
    callTwitter (lookupSearchReplies user minId) & fmap $
      (^. searchResultStatuses)
        >>> filter interacted
        >>> foldMap collectTweet

  mentions <-
    streamTwitter (lookupMentions minId) & fmap $
      (filter interacted)
        >>> foldMap collectTweet

  let
    tweets' = tweets <> replies <> mentions

  logDebug "##########"
  logDebug $ "Found " <> (pack $ show $ length tweets') <> " tweets"

  let
    next = parents tweets'


  (_, tweets'') <- execStateT processTw (next <> truncated tweets', tweets')

  let
    (orphans, roots) = M.partition hasParent tweets''

  tweetTree <- evalStateT (unfoldForestM buildTweetTree (M.elems roots)) orphans

  cfg <- liftIO defaultRenderConfig

  doc <- processRenderT (renderTweets tweetTree) cfg $ whenM (config @"downloadMedia") fetchImages

  config @"format" >>= \case
    Markdown ->
      liftIO $ Pandoc.runIOorExplode $ Pandoc.writeMarkdown def doc
        >>= writeOutputFile "./tweets.md"
    Html ->
      liftIO $ Pandoc.runIOorExplode $ Pandoc.writeHtml5String def doc
        >>= writeOutputFile "./tweets.html"
    Org ->
      liftIO $ Pandoc.runIOorExplode $ Pandoc.writeOrg def doc
        >>= writeOutputFile "./tweets.org"

  pure ()

writeOutputFile :: FilePath -> T.Text -> m ()
writeOutputFile name tx = do
  path <- config @"outDir"
  liftIO $ T.writeFile (path </> name) tx

fetchImages :: (MonadApp '[] m) => RenderOutputs -> m ()
fetchImages outs = for_ (imageOutputs outs) $ \img -> do
  let url = imageInfoUrl img
  dir <- config @"outDir"
  let imagesDir = dir </> "images"
      file = T.unpack $ imageName $ imageInfoUrl img
  liftIO $ createDirectoryIfMissing False (dir </> "images")
  exists <- liftIO $ doesFileExist file
  unless exists $ do
    logNormal ("Fetching image at" <> url)
    callTwitter (fetchImage img)
      >>= liftIO . LBS.writeFile file . imgBytes

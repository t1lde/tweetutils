module TweetUtils.Commands.DumpTweets
    ( FormatType (..)
    , apiDumpTweets
    , type DumpTweetInfo
    ) where

--------------------------------------------------------------------------------

import Control.Arrow
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Foldable          (for_)
import GHC.Generics           qualified as GHC

--------------------------------------------------------------------------------

import Control.Lens          hiding (children)
import Control.Monad.Extra   (whenM)
import Data.ByteString.Lazy  qualified as LBS
import Data.Default          (Default (def))
import Data.Map              qualified as M
import Data.Text             qualified as T
import Data.Text.IO          qualified as T
import Options.Generic       (ParseField (..), ParseFields (..), ParseRecord (..))
import System.Directory      (createDirectoryIfMissing, doesFileExist)
import System.FilePath.Posix ((</>))

--------------------------------------------------------------------------------

import Conduit                    ()
import Control.Monad.State.Strict (evalStateT, execStateT)
import Data.Tree
import Text.Pandoc.Class          qualified as Pandoc
import Text.Pandoc.Writers        qualified as Pandoc
import Web.Twitter.Types.Lens     (User, searchResultStatuses)

--------------------------------------------------------------------------------

import TweetUtils.Image    (fetchImage, imgBytes)
import TweetUtils.MonadApp (MonadApp, Named ((:=)), callTwitter, config, logDebug, logNormal,
                            streamTwitter)
import TweetUtils.Query    (buildTweetTree, collectTweet, hasParent, interacted, lookupMentions,
                            lookupSearchReplies, parents, processTw, showUser, truncated, userTL)
import TweetUtils.Render   (RenderOutputs (..), defaultRenderConfig, imageInfoUrl, imageName,
                            processRenderT, renderTweets)

--------------------------------------------------------------------------------

type DumpTweetInfo =
   '[ "outDir" ':= FilePath
    , "format"':= FormatType
    , "downloadMedia" ':= Bool
    ]

data FormatType
  = Markdown
  | Html
  | Org
  deriving stock (GHC.Generic, Read, Show)
  deriving anyclass (ParseField, ParseFields, ParseRecord)

--------------------------------------------------------------------------------

apiDumpTweets ∷ forall m.
  ( MonadApp DumpTweetInfo m
  ) ⇒
  User → m ()
apiDumpTweets user = do
  --logNormal "Verifying access credentials..."
  --user <- callTwitter accountVerifyCredentials
  logNormal $ "Authenticated as user " <> showUser user

  tweets <-
    streamTwitter (userTL user)
      & fmap (foldMap collectTweet)

  let
    minId = maybe 0 fst $ M.lookupMin tweets

  replies <-
    callTwitter (lookupSearchReplies user minId)
      <&>
      ((^. searchResultStatuses)
        >>> filter interacted
        >>> foldMap collectTweet)

  mentions <-
    streamTwitter (lookupMentions minId)
      <&> (filter interacted >>> foldMap collectTweet)

  let
    tweets' = tweets <> replies <> mentions

  logDebug "##########"
  logDebug $ "Found " <> T.pack (show $ length tweets') <> " tweets"

  let
    next = parents tweets'


  (_, tweets'') <- execStateT processTw (next <> truncated tweets', tweets')

  let
    (orphans, roots) = M.partition hasParent tweets''

  tweetTree <- evalStateT (unfoldForestM buildTweetTree (M.elems roots)) orphans

  cfg <- liftIO defaultRenderConfig

  doc <- processRenderT (renderTweets tweetTree) cfg $ whenM (config @"downloadMedia") . fetchImages

  config @"format" >>= \case
    Markdown ->
      liftIO (Pandoc.runIOorExplode $ Pandoc.writeMarkdown def doc)
        >>= writeOutputFile "./tweets.md"
    Html ->
      liftIO (Pandoc.runIOorExplode $ Pandoc.writeHtml5String def doc)
        >>= writeOutputFile "./tweets.html"
    Org ->
      liftIO (Pandoc.runIOorExplode $ Pandoc.writeOrg def doc)
        >>= writeOutputFile "./tweets.org"

  pure ()

writeOutputFile ∷ (MonadApp DumpTweetInfo m) ⇒ FilePath → T.Text → m ()
writeOutputFile name tx = do
  path <- config @"outDir"
  liftIO $ T.writeFile (path </> name) tx

fetchImages ∷ (MonadApp DumpTweetInfo m) ⇒ RenderOutputs → m ()
fetchImages outs = for_ (imageOutputs outs) $ \img -> do
  let url = imageInfoUrl img
  dir <- config @"outDir"
  let imagesDir = dir </> "images"
      file = imagesDir </> T.unpack (imageName $ imageInfoUrl img)
  liftIO $ createDirectoryIfMissing False imagesDir
  exists <- liftIO $ doesFileExist file
  unless exists $ do
    logNormal ("Fetching image at " <> url)
    callTwitter (fetchImage img)
      >>= liftIO . LBS.writeFile file . imgBytes

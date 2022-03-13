module Query (apiDelete)  where

import Conduit
import Control.Lens hiding (children)
import Control.Monad.State.Class
import Web.Twitter.Conduit
import Web.Twitter.Types.Lens hiding (user)
import Data.ByteString.Lazy qualified as LBS
--import Data.Conduit
--import qualified Data.Conduit.List as CL
--import Control.Concurrent.ParallelIO

--import Data.Time.Clock
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text hiding (length, minimum, null, filter)
import qualified Data.Text.IO as T
import System.Directory (doesFileExist)

--import System.IO (hFlush, stdout)
--import Data.Function
--import Data.List (minimumBy)
import Text.Pandoc.Writers qualified as Pandoc
import Text.Pandoc.Class qualified as Pandoc
import Data.Maybe (fromMaybe, isJust)
import Data.Tree
import Control.Arrow
import Control.Monad
import Control.Applicative
import Control.Monad.State.Lazy (execStateT, evalStateT)
--import Control.Monad.IO.Class (MonadIO (liftIO))
--import Options
import Render 
  ( renderTweets, defaultRenderConfig, RenderOutputs (..), processAppT, imageInfoUrl
  , imageName
  )
import Image (fetchImage, imgBytes)
import Data.Foldable (for_)

apiDelete :: TWInfo -> Manager -> IO ()
apiDelete info mgr = do
  T.putStrLn $ "Verifying access credentials..."
  user <- call info mgr $ accountVerifyCredentials
  T.putStrLn $ "Authenticated as user " <> (showUser user)
  --putStrLn $ showModeQuery opts

  tweets <- 
    fmap (foldMap collectTweet) $ runConduit $ 
      sourceWithMaxId info mgr (userTL user)
       .| sinkList

  let 
    minId = maybe 0 fst $ M.lookupMin tweets

  replies <-
    call info mgr (lookupSearchReplies user minId)
      & fmap (^. searchResultStatuses)
      & fmap (filter interacted)
      & fmap (foldMap collectTweet)

  mentions <- 
    sourceWithMaxId info mgr (lookupMentions minId)
      & (.| filterC interacted .| sinkList)
      & runConduit
      & fmap (foldMap collectTweet)
  
  let
    tweets' = tweets <> replies <> mentions
  
  T.putStrLn "##########"
  T.putStrLn $ "Found " <> (pack $ show $ length tweets') <> " tweets"

  let 
    next = parents tweets'

  T.putStrLn $ "with " <> (pack $ show $ length next) <> " parents"
  T.putStrLn $ (pack $ show $ length replies) <> " replies"
  T.putStrLn $ (pack $ show $ length mentions) <> " mentions"


  (_, tweets'') <- execStateT (processTw info mgr) (next <> truncated tweets', tweets')

  T.putStrLn $ 
    (pack $ show $ length $ truncated tweets'') <> " truncated" 
  
  let
    (orphans, roots) = M.partition hasParent tweets''

  tweetTree <- evalStateT (unfoldForestM buildTweetTree (M.elems roots)) orphans

  cfg <- defaultRenderConfig

  doc <- 
    processAppT (renderTweets tweetTree) cfg $ \outs -> do
      T.writeFile "./images/urls.txt" $
        foldMap (\img -> imageInfoUrl img <> "\n") (imageOutputs outs)

      for_ (imageOutputs outs) $ \img -> do
        let url = imageInfoUrl img
        let file = unpack $ "./images/" <> imageName url
        exists <- doesFileExist file
        unless exists (do
            T.putStrLn $ "Fetching image at " <> url
            call info mgr (fetchImage img)
              >>= (LBS.writeFile file . imgBytes))


  (Pandoc.runIOorExplode $ Pandoc.writeMarkdown def doc)
      >>= T.writeFile "./tweets.md"

  (Pandoc.runIOorExplode $ Pandoc.writeHtml5String def doc)
      >>= T.writeFile "./tweets.html"

  (Pandoc.runIOorExplode $ Pandoc.writeOrg def doc)
      >>= T.writeFile "./tweets.org"

  (Pandoc.runIOorExplode $ Pandoc.writeMan def doc)
      >>= T.writeFile "./tweets.man"

  pure ()

interacted :: Status -> Bool
interacted s =
  fromMaybe False $ liftA2 (||) (s ^. statusFavorited) (s ^. statusRetweeted)

userTL :: User -> APIRequest StatusesUserTimeline [Status]
userTL user = 
  statusesUserTimeline (UserIdParam $ user ^. userId) 
    & #count ?~ 100
    & #contributor_details ?~ True
    & #exclude_replies ?~ False
    & #tweet_mode ?~ Extended

processTw :: 
  ( MonadIO m
  , MonadState (S.Set StatusId, M.Map StatusId Status) m
  ) => 
  TWInfo -> Manager -> m ()
processTw info mgr = do
  (next, all_tweets) <- get

  liftIO $ T.putStrLn $ 
    "Searching for " <> (pack $ show $ length next) 
    <> " parents from " <> (pack $ show $ length all_tweets) <> " total tweets"

  tweets <- 
    lookupTweets (next <> truncated all_tweets)
      & call info mgr
      & fmap (foldMap collectTweet)
      & liftIO
      
  let
    tweets' = (tweets <> all_tweets)
    tweets'' = (tweets' <> foldMap quoteParent tweets')

    next' = parents tweets''

  put (next', tweets'')

  unless (null next') $ processTw info mgr

truncated :: M.Map StatusId Status -> S.Set StatusId
truncated xs = foldMap ((^. statusId) >>> S.singleton) $ M.filter (\s -> s ^. statusTruncated) xs

buildTweetTree :: 
  (MonadState (M.Map StatusId Status) m) => Status -> m (Status, [Status])
buildTweetTree tw = do
  children <- gets $ M.filter $ childOf tw
  modify (M.\\ children)
  pure (tw, M.elems children)

hasParent :: Status -> Bool
hasParent status = 
  isJust (status ^. statusRetweetedStatus) 
    || isJust (status ^. statusQuotedStatus) 
    || isJust (status ^. statusQuotedStatusId)
    || isJust (status ^. statusInReplyToStatusId) 
    || isJust (status ^. statusCurrentUserRetweet)

childOf :: Status -> Status -> Bool
childOf parent status = 
  (((status ^. statusRetweetedStatus) & fmap (^. statusId)) == parentId)
    || (((status ^. statusQuotedStatus) & fmap (^. statusId)) == parentId)
    || (status ^. statusQuotedStatusId == parentId)
    || (status ^. statusInReplyToStatusId == parentId)
    || (status ^. statusCurrentUserRetweet == parentId)
  where
    parentId = Just $ parent ^. statusId

  

lookupTweets :: S.Set StatusId -> APIRequest StatusesLookup [Status]
lookupTweets tws = 
  statusesLookup (S.toList tws) 
    & #include_entities ?~ True
    & #tweet_mode ?~ Extended

lookupSearchReplies :: User -> StatusId -> APIRequest SearchTweets (SearchResult [Status])
lookupSearchReplies user minId = 
  searchTweets ("to:" <> user ^. userScreenName)
    & #include_entities ?~ True
    & (#since_id ?~ minId)
    & #tweet_mode ?~ Extended

lookupMentions :: StatusId -> APIRequest StatusesMentionsTimeline [Status]
lookupMentions minId = 
  statusesMentionsTimeline  
     & #include_entities ?~ True
     & #contributor_details ?~ True
     & #since_id ?~ minId
     & #tweet_mode ?~ Extended

parents :: M.Map StatusId Status -> S.Set StatusId
parents xs = 
  xs 
    & foldMap ((^. statusInReplyToStatusId) >>> maybe mempty S.singleton)
    & S.filter ((`M.member` xs) >>> not)

quoteParent :: Status -> M.Map StatusId Status
quoteParent tw = 
  (maybe mempty collectTweet $ (tw ^. statusRetweetedStatus) <|> (tw ^. statusQuotedStatus)) 

collectTweet :: Status -> M.Map StatusId Status
collectTweet tw = M.singleton (tw ^. statusId) tw

--queryLikes :: TWInfo -> Manager -> CliDeleteOptions UTCTime -> User -> IO [Status]
--queryLikes info mgr opts user | cliDeleteLikes opts == CliDeleteLikes = do
--  putStrLn "Querying Likes..."
--  likes <- runConduit $ sourceWithMaxId info mgr (favoritesList (pure $ UserIdParam $ user^.userId ) & #count ?~ 200)
--    .| CL.filter ((^.statusCreatedAt) >>> (deleteFilter opts))
--    .| CL.consume
--  putStrLn $ showQueryLikesSummary likes
--  return likes
--queryLikes _ _ _ _ | otherwise = return $ mempty
--
--deleteTweet :: TWInfo -> Manager -> Status -> IO ()
--deleteTweet info mgr tweet = do
--  tweet' <- call info mgr $ statusesDestroyId (tweet ^. statusId)
--  putStrLn $ "Successfully deleted tweet "<>(show $ tweet' ^. statusId)
--
--deleteLike :: TWInfo -> Manager -> Status -> IO ()
--deleteLike info mgr tweet = do
--  tweet' <- call info mgr $ favoritesDestroy (tweet ^. statusId)
--  putStrLn $ "Successfully deleted like on tweet "<>(show $ tweet'^.statusId)
--
--showQuerySummary :: [Status] -> String
--showQuerySummary [] = "No tweets found."
--showQuerySummary tweets =
--  "Found " <> (show $ length tweets) <> " tweets, oldest is "
--  <> (show $ oldest^.statusId) <>" created at "<> (show $ oldest^.statusCreatedAt)
--  where
--    oldest :: Status
--    oldest = minimumBy (compare `on` (^.statusCreatedAt)) tweets
--
--showQueryLikesSummary :: [Status] -> String
--showQueryLikesSummary [] = "No liked tweets found."
--showQueryLikesSummary likes =
--  "Found " <> (show $ length likes) <> " liked tweets, oldest is "
--  <> (show $ oldest^.statusId) <>" created at "<> (show $ oldest^.statusCreatedAt)
--  where
--    oldest :: Status
--    oldest = minimumBy (compare `on` (^.statusCreatedAt)) likes
--
--promptCont :: CliPromptMode -> IO () -> IO ()
--promptCont CliConfirm cont = do
--  putStr $ "Delete tweets (yes)/no:"
--  hFlush stdout
--  ans <- getLine
--  unless (ans == "no") cont
--promptCont CliForce cont = cont
--promptCont CliDryRun _ = return ()
--
showUser :: User -> Text
showUser user = user^.userScreenName<>" @"<>user^.userName
--
--showModeQuery :: (Show time) => CliDeleteOptions time -> String
--showModeQuery opts = case cliDeleteMode opts of
--  (DeleteAllMode) -> "Querying all tweets..."
--  (FromDateMode param time) -> "Querying tweets posted before "<>(param)<>"("<>(show time)<>") ..."
--  (BeforeDurationMode param time) -> "Querying tweets older than "<>(param)<>"( posted before "<>(show time)<>") ..."
--
--deleteFilter :: (Ord time) => CliDeleteOptions time -> time -> Bool
--deleteFilter opts time = case cliDeleteMode opts of
--  (DeleteAllMode) -> True
--  (FromDateMode _ beforetime) -> time < beforetime
--  (BeforeDurationMode _ beforetime) -> time < beforetime
--
module TweetUtils.Query
    ( -- * Queries/Tweet processing
      buildTweetTree
    , lookupMentions
    , lookupSearchReplies
    , lookupTweets
    , processTw
    , userTL
      -- * Misc Utils
    , childOf
    , collectTweet
    , hasParent
    , interacted
    , parents
    , quoteParent
    , showUser
    , truncated
    ) where

--------------------------------------------------------------------------------

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe          (isJust)

--------------------------------------------------------------------------------

import Control.Lens               hiding (children)
import Control.Monad.State.Strict (StateT, get, gets, modify, put)
import Control.Monad.Trans        (lift)
import Data.Map                   qualified as M
import Data.Set                   qualified as S
import Data.Text                  hiding (filter, length, minimum, null)

--------------------------------------------------------------------------------

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens hiding (user)

--------------------------------------------------------------------------------

import TweetUtils.MonadApp (MonadAppBase, callTwitter, logDebug)

--------------------------------------------------------------------------------


interacted ∷ Status → Bool
interacted s =
  Just True == liftA2 (||) (s ^. statusFavorited) (s ^. statusRetweeted)

userTL ∷ User → APIRequest StatusesUserTimeline [Status]
userTL user =
  statusesUserTimeline (UserIdParam $ user ^. userId)
    & #count ?~ 100
    & #contributor_details ?~ True
    & #exclude_replies ?~ False
    & #tweet_mode ?~ Extended

processTw ∷
  ( MonadAppBase m
  ) ⇒
  StateT (S.Set StatusId, M.Map StatusId Status) m ()
processTw = do
  (next, all_tweets) <- get

  lift $ logDebug $
    "Searching for " <> pack (show $ length next)
      <> " parents from " <> pack (show $ length all_tweets) <> " total tweets"

  tweets <-
    lift (callTwitter $ lookupTweets $ next <> truncated all_tweets)
      & fmap (foldMap collectTweet)

  let
    tweets' = tweets <> all_tweets
    tweets'' = tweets' <> foldMap quoteParent tweets'

    next' = parents tweets''

  put (next', tweets'')

  unless (null next') processTw

truncated ∷ M.Map StatusId Status → S.Set StatusId
truncated xs = foldMap ((^. statusId) >>> S.singleton) $ M.filter (^. statusTruncated) xs

buildTweetTree ∷ (Monad m) ⇒
  Status → StateT (M.Map StatusId Status) m (Status, [Status])
buildTweetTree tw = do
  children <- gets $ M.filter $ childOf tw
  modify (M.\\ children)
  pure (tw, M.elems children)

hasParent ∷ Status → Bool
hasParent status =
  isJust (status ^. statusRetweetedStatus)
    || isJust (status ^. statusQuotedStatus)
    || isJust (status ^. statusQuotedStatusId)
    || isJust (status ^. statusInReplyToStatusId)
    || isJust (status ^. statusCurrentUserRetweet)

childOf ∷ Status → Status → Bool
childOf parent status =
  ((status ^. statusRetweetedStatus) & fmap (^. statusId)) == parentId
    || ((status ^. statusQuotedStatus) & fmap (^. statusId)) == parentId
    || status ^. statusQuotedStatusId == parentId
    || status ^. statusInReplyToStatusId == parentId
    || status ^. statusCurrentUserRetweet == parentId
  where
    parentId = Just $ parent ^. statusId

lookupTweets ∷ S.Set StatusId → APIRequest StatusesLookup [Status]
lookupTweets tws =
  statusesLookup (S.toList tws)
    & #include_entities ?~ True
    & #tweet_mode ?~ Extended

lookupSearchReplies ∷ User → StatusId → APIRequest SearchTweets (SearchResult [Status])
lookupSearchReplies user minId =
  searchTweets ("to:" <> user ^. userScreenName)
    & #include_entities ?~ True
    & (#since_id ?~ minId)
    & #tweet_mode ?~ Extended

lookupMentions ∷ StatusId → APIRequest StatusesMentionsTimeline [Status]
lookupMentions minId =
  statusesMentionsTimeline
     & #include_entities ?~ True
     & #contributor_details ?~ True
     & #since_id ?~ minId
     & #tweet_mode ?~ Extended

parents ∷ M.Map StatusId Status → S.Set StatusId
parents xs =
  xs
    & foldMap ((^. statusInReplyToStatusId) >>> maybe mempty S.singleton)
    & S.filter ((`M.member` xs) >>> not)

quoteParent ∷ Status → M.Map StatusId Status
quoteParent tw =
  maybe mempty collectTweet $ (tw ^. statusRetweetedStatus) <|> (tw ^. statusQuotedStatus)

collectTweet ∷ Status → M.Map StatusId Status
collectTweet tw = M.singleton (tw ^. statusId) tw

showUser ∷ User → Text
showUser user = user^.userScreenName<>" @"<>user^.userName

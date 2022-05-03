module TweetUtils.Render
    ( ImageInfo (..)
    , RenderConfig (..)
    , RenderOutputs (..)
    , RenderT (..)
    , defaultRenderConfig
    , imageInfoUrl
    , imageName
    , processRenderT
    , renderTweet
    , renderTweets
    , runRenderT
    ) where

import Control.Applicative        (liftA2)
import Control.Lens               ((^.))
import Control.Monad.IO.Class     (MonadIO (..))
import Control.Monad.RWS.Strict   (RWST, evalRWST)
import Control.Monad.Reader.Class (MonadReader (..), asks)
import Control.Monad.Trans.Class  (MonadTrans)
import Control.Monad.Writer.Class (MonadWriter (..), tell)
import Data.Foldable              (fold)
import Data.Function              (on)
import Data.Functor               (($>))
import Data.List                  (sortBy)
import Data.Maybe                 (isJust)
import Data.Ord                   (Down (Down))
import Data.Set                   qualified as S
import Data.Text                  qualified as T
import Data.Time.Clock            (UTCTime, getCurrentTime)
import Data.Time.Format           (TimeLocale, defaultTimeLocale, formatTime)
import Data.Time.LocalTime        (TimeZone, getCurrentTimeZone, utcToZonedTime)
import Data.Tree                  (Tree (..))
import Text.Pandoc.Builder        (Blocks, Inlines, doc, emph, header, image, linebreak, link, para,
                                   space, text)
import Text.Pandoc.Definition     (Pandoc (..))
import Web.Twitter.Types          (Entities (..), Entity (..), ExtendedEntities (..),
                                   ExtendedEntity (..), HashTagEntity (..), MediaEntity (..),
                                   URLEntity (..), Variant (..), type URIString, vsVariants)
import Web.Twitter.Types.Lens     (Status, User, statusCreatedAt, statusEntities,
                                   statusExtendedEntities, statusFavoriteCount, statusId,
                                   statusInReplyToStatusId, statusQuotedStatusId,
                                   statusRetweetCount, statusRetweetedStatus, statusText,
                                   statusUser, userName, userScreenName)

data RenderConfig
  = RenderConfig
      { dateLocale  :: TimeLocale
      , dateFmt     :: String
      , currentTime :: UTCTime
      , timeZone    :: TimeZone
      }

data ImageInfo
  = ImageInfo
      { imageUrl :: T.Text
      }
  | VideoInfo
      { videoUrl      :: T.Text
      , videoVariants :: [T.Text]
      }
  deriving stock (Eq, Ord)

imageInfoUrl ∷ ImageInfo → T.Text
imageInfoUrl (ImageInfo u)         = u
imageInfoUrl (VideoInfo u [])      = u
imageInfoUrl (VideoInfo _ (u : _)) = u

imageName ∷ URIString → T.Text
imageName url =
  fst $ T.breakOn "?" (snd $ T.breakOnEnd "/" url)

newtype RenderOutputs
  = RenderOutputs { imageOutputs :: S.Set ImageInfo }
  deriving newtype (Monoid, Semigroup)

type MonadRender m =
  ( MonadReader RenderConfig m
  , MonadWriter RenderOutputs m
  )

newtype RenderT m a
  = RenderT { unRenderT :: RWST RenderConfig RenderOutputs () m a }
  deriving newtype
    ( Applicative
    , Functor
    , Monad
    , MonadReader RenderConfig
    , MonadTrans
    , MonadWriter RenderOutputs
    )

runRenderT ∷ (Monad m) ⇒ RenderT m a → RenderConfig → m (a, RenderOutputs)
runRenderT (RenderT app) cfg =
  evalRWST app cfg ()

processRenderT ∷ (Monad m) ⇒
  RenderT m a → RenderConfig → (RenderOutputs → m ()) → m a
processRenderT app cfg f = do
  (r, outs) <- runRenderT app cfg
  f outs $> r

defaultRenderConfig ∷ (MonadIO m) ⇒ m RenderConfig
defaultRenderConfig =
  RenderConfig
  <$> pure defaultTimeLocale
  <*> pure "%Y-%m-%d %H:%M:%S"
  <*> liftIO getCurrentTime
  <*> liftIO getCurrentTimeZone

foldMapM ∷
  (Traversable t, Applicative f, Monoid m) ⇒
  (a → f m) → t a → f m
foldMapM f = fmap (fold) . traverse f

flatSequence ∷
  (Traversable t, Applicative f, Monoid m) ⇒
  t (f m) → f m
flatSequence = fmap (fold) . sequenceA

renderTweets ∷
  (MonadRender m) ⇒
  [Tree Status] → m Pandoc
renderTweets tweets =
  doc <$>
    foldMapM (renderTweet 1) tweets

renderTweet ∷
  (MonadRender m) ⇒
  Int → Tree Status → m Blocks
renderTweet level t@(Node tw children)
  | level == 1 =
      liftA2 (<>)
        (renderTopTweet tw level)
        (renderTweet (succ level) t)
  | otherwise =
      liftA2 (<>)
        (renderTweet' level tw)
        (foldMapM (renderTweet $ succ level) children)

renderTweet' ∷
  (MonadRender m) ⇒
   Int → Status → m Blocks
renderTweet' level tw
  | isRetweet tw = do
      time <- renderTime $ tw ^. statusCreatedAt
      (header level) <$> flatSequence
        [ pure $ renderUser $ tw ^. statusUser
        , pure $ text (" retweeted at " <> time)
        , pure linebreak
        ]
  | otherwise = do

  time <- renderTime $ tw ^. statusCreatedAt

  flatSequence
    [ (header level) <$> flatSequence
        [ pure $ renderUser $ tw ^. statusUser
        , pure $ text (tweetType <> " " <> time)
        , pure linebreak
        ]

    , pure $ para $ text $ tw ^. statusText

    , maybe
        (pure mempty)
        (renderEntities (tw ^. statusExtendedEntities))
        (tw ^. statusEntities)

    , pure $ para
        ((showText $ retweets)
             <> (pluralise retweets "retweet" "retweets")
             <> (showText $ likes)
             <> (pluralise likes "like" "likes"))
    ]
    where
      retweets = tw ^. statusRetweetCount
      likes = tw ^. statusFavoriteCount
      tweetType
        | isReply tw = " replied at "
        | isQuote tw = " quote-tweeted at "
        | otherwise = " posted at "
      pluralise n a b
        | n == 1    = text $ " " <> a <> " "
        | otherwise = text $ " " <> b <> " "


showText ∷ (Show a) ⇒ a → Inlines
showText = text . T.pack . show

isRetweet ∷ Status → Bool
isRetweet tw =
  isJust $ tw ^. statusRetweetedStatus

isReply ∷ Status → Bool
isReply tw =
  isJust $ tw ^. statusInReplyToStatusId

isQuote ∷ Status → Bool
isQuote tw =
  isJust $ tw ^. statusQuotedStatusId

renderEntities ∷ (MonadRender m) ⇒ Maybe ExtendedEntities → Entities →  m Blocks
renderEntities Nothing (Entities tags _mentions urls media) =
  flatSequence
    [ maybeFoldEntity para renderMediaEnt media
    , maybeFoldEntity para (pure . renderUrlEnt) urls
    , maybeFoldEntity para (pure . hashTagLink) tags
    ]
renderEntities (Just (ExtendedEntities ext)) (Entities tags _mentions urls _media) =
  flatSequence
    [ maybeFoldEntity para renderExtEnt ext
    , maybeFoldEntity para (pure . renderUrlEnt) urls
    , maybeFoldEntity para (pure . hashTagLink) tags
    ]


renderMediaEnt ∷ (MonadRender m) ⇒ MediaEntity → m Inlines
renderMediaEnt ent = do
  tell $ RenderOutputs $ S.singleton img

  pure $
    image path "" ""
      <> linebreak
      <> renderUrlEnt (meURL ent)
  where
    img = ImageInfo url

    path = "./images/" <> imageName (imageInfoUrl img)

    url = meMediaURLHttps ent

renderExtEnt ∷ (MonadRender m) ⇒ ExtendedEntity → m Inlines
renderExtEnt ent = do
  tell $ RenderOutputs $ S.singleton img'

  pure $
    image path "" ""
      <> linebreak
      <> renderUrlEnt (exeURL ent)
  where
    url = exeMediaUrlHttps ent
    path = "./images/" <> imageName (imageInfoUrl img')
    img' = img (exeVideoInfo ent)
    img Nothing = ImageInfo url
    img (Just vInfo) =
      VideoInfo url $
        fmap vUrl
          (sortBy (compare `on` Down . vBitrate) $ vsVariants vInfo)


renderUrlEnt ∷ URLEntity → Inlines
renderUrlEnt (URLEntity u expanded disp) =
  link entUrl "" $ text disp
  where
    entUrl
      | "https://t.co/" `T.isPrefixOf` u = expanded
      | otherwise = u

maybeFoldEntity ∷
  ( Monoid u
  , Monoid v
  , MonadRender m
  ) ⇒
  (u → v) → (a → m u) → [Entity a] → m v
maybeFoldEntity _outer _inner [] = pure mempty
maybeFoldEntity outer inner xs =
  fmap outer $ foldMapM inner $ fmap entityBody xs


hashTagLink ∷ HashTagEntity → Inlines
hashTagLink (HashTagEntity tx) =
  link (hashTagUrl tx) "" (text $ "#" <> tx)
    <> space

hashTagUrl ∷ T.Text → T.Text
hashTagUrl tx =
 "https://twitter.com/hashtag/"
   <> tx

renderTopTweet ∷
  (MonadRender m) ⇒
  Status → Int → m Blocks
renderTopTweet tw level =
  fmap (header level) $ flatSequence
  [ pure $ text "Twitter thread at "
  , pure $ tweetLink tw
  , pure $ text " from "
  , text <$> (renderTime $ tw ^. statusCreatedAt)
  , pure $ text " imaged at "
  , text <$> renderCurrentTime
  ]

renderCurrentTime ∷ (MonadRender m) ⇒ m T.Text
renderCurrentTime =
  asks currentTime >>= renderTime

renderTime ∷ (MonadRender m) ⇒ UTCTime → m T.Text
renderTime t = do
  locale <- asks dateLocale
  fmt <- asks dateFmt
  zone <- asks timeZone
  pure $
   T.pack $ formatTime locale fmt
    (utcToZonedTime zone t)


tweetLink ∷ Status → Inlines
tweetLink = bareLink . tweetUrl

renderUser ∷ User → Inlines
renderUser u =
 userLink u <> space <> emph (text $ "@" <> u ^. userScreenName)

userLink ∷ User → Inlines
userLink u =
  link (userUrl u) "" $ text $ u ^. userName


bareLink ∷ T.Text → Inlines
bareLink url = link url "" $ text url

tweetUrl ∷ Status → T.Text
tweetUrl tw =
  userUrl (tw ^. statusUser)
    <> "/status/"
    <> (T.pack . show) (tw ^. statusId)

userUrl ∷ User → T.Text
userUrl u =
  "https://twitter.com/"
    <> u ^. userScreenName







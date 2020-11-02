module Delete (apiDelete) where

import Control.Lens

import Web.Twitter.Conduit
import Web.Twitter.Types.Lens hiding (user)
import Data.Conduit
import qualified Data.Conduit.List as CL

import Data.Time.Clock
import Data.Text hiding (length, minimum)
import qualified Data.Text.IO as T

import System.IO (hFlush, stdout)
import Data.Function
import Data.List (minimumBy)
import Control.Arrow
import Control.Monad

import Options

apiDelete :: TWInfo -> Manager -> CliDeleteOptions UTCTime -> IO ()
apiDelete info mgr opts = do
  T.putStrLn $ "Verifying access credentials..."
  user <- call info mgr $ accountVerifyCredentials
  T.putStrLn $ "Authenticated as user " <> (showUser user)
  putStrLn $ showModeQuery opts
  tweets <- runConduit $ sourceWithMaxId info mgr (statusesUserTimeline (UserIdParam $ user^.userId) & #count ?~ 200)
    .| CL.filter ((^.statusCreatedAt) >>> (deleteFilter opts))
    .| CL.consume
  putStrLn $ showQuerySummary tweets
  promptCont (cliPromptMode opts) $ mapM_ (deleteTweet info mgr) tweets

deleteTweet :: TWInfo -> Manager -> Status -> IO ()
deleteTweet info mgr tweet = do
  tweet' <- call info mgr $ statusesDestroyId (tweet ^. statusId)
  putStrLn $ "Successfully deleted tweet "<>(show $ tweet' ^. statusId)

showQuerySummary :: [Status] -> String
showQuerySummary tweets =
  "Found " <> (show $ length tweets) <> " tweets, oldest is "
  <> (show $ oldest^.statusId) <>" created at "<> (show $ oldest^.statusCreatedAt)
  where
    oldest :: Status
    oldest = minimumBy (compare `on` (^.statusCreatedAt)) tweets

promptCont :: CliPromptMode -> IO () -> IO ()
promptCont CliConfirm cont = do
  putStr $ "Delete tweets (yes)/no:"
  hFlush stdout
  ans <- getLine
  unless (ans == "no") cont
promptCont CliForce cont = cont
promptCont CliDryRun _ = return ()

showUser :: User -> Text
showUser user = user^.userScreenName<>" @"<>user^.userName

showModeQuery :: (Show time) => CliDeleteOptions time -> String
showModeQuery opts = case cliDeleteMode opts of
  (DeleteAllMode) -> "Querying all tweets..."
  (FromDateMode param time) -> "Querying tweets posted before "<>(param)<>"("<>(show time)<>") ..."
  (BeforeDurationMode param time) -> "Querying tweets older than "<>(param)<>"( posted before "<>(show time)<>") ..."

deleteFilter :: (Ord time) => CliDeleteOptions time -> time -> Bool
deleteFilter opts time = case cliDeleteMode opts of
  (DeleteAllMode) -> True
  (FromDateMode _ beforetime) -> time < beforetime
  (BeforeDurationMode _ beforetime) -> time < beforetime
